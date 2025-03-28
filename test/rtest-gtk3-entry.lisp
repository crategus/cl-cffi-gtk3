(in-package :gtk-test)

(def-suite gtk-entry :in gtk-suite)
(in-suite gtk-entry)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryIconPosition

(test gtk-entry-icon-position
  ;; Check type
  (is (g:type-is-enum "GtkEntryIconPosition"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEntryIconPosition")
          (g:gtype (cffi:foreign-funcall "gtk_entry_icon_position_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:entry-icon-position
          (glib:symbol-for-gtype "GtkEntryIconPosition")))
  ;; Check names
  (is (equal '("GTK_ENTRY_ICON_PRIMARY" "GTK_ENTRY_ICON_SECONDARY")
             (glib-test:list-enum-item-names "GtkEntryIconPosition")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkEntryIconPosition")))
  ;; Check nick names
  (is (equal '("primary" "secondary")
             (glib-test:list-enum-item-nicks "GtkEntryIconPosition")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkEntryIconPosition"
                                    GTK:ENTRY-ICON-POSITION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_entry_icon_position_get_type")
                       (:PRIMARY 0)
                       (:SECONDARY 1))
             (gobject:get-gtype-definition "GtkEntryIconPosition"))))

;;;     GtkInputPurpose

(test gtk-input-purpose
  ;; Check type
  (is (g:type-is-enum "GtkInputPurpose"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInputPurpose")
          (g:gtype (cffi:foreign-funcall "gtk_input_purpose_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:input-purpose
          (glib:symbol-for-gtype "GtkInputPurpose")))
  ;; Check names
  (is (equal '("GTK_INPUT_PURPOSE_FREE_FORM" "GTK_INPUT_PURPOSE_ALPHA"
               "GTK_INPUT_PURPOSE_DIGITS" "GTK_INPUT_PURPOSE_NUMBER"
               "GTK_INPUT_PURPOSE_PHONE" "GTK_INPUT_PURPOSE_URL"
               "GTK_INPUT_PURPOSE_EMAIL" "GTK_INPUT_PURPOSE_NAME"
               "GTK_INPUT_PURPOSE_PASSWORD" "GTK_INPUT_PURPOSE_PIN"
               "GTK_INPUT_PURPOSE_TERMINAL")
             (glib-test:list-enum-item-names "GtkInputPurpose")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10)
             (glib-test:list-enum-item-values "GtkInputPurpose")))
  ;; Check nick names
  (is (equal '("free-form" "alpha" "digits" "number" "phone" "url" "email"
               "name" "password" "pin" "terminal")
             (glib-test:list-enum-item-nicks "GtkInputPurpose")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkInputPurpose" GTK:INPUT-PURPOSE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_input_purpose_get_type")
                       (:FREE-FORM 0)
                       (:ALPHA 1)
                       (:DIGITS 2)
                       (:NUMBER 3)
                       (:PHONE 4)
                       (:URL 5)
                       (:EMAIL 6)
                       (:NAME 7)
                       (:PASSWORD 8)
                       (:PIN 9)
                       (:TERMINAL 10))
             (gobject:get-gtype-definition "GtkInputPurpose"))))

;;;     GtkInputHints

(test gtk-input-hints
  ;; Check type
  (is (g:type-is-flags "GtkInputHints"))
  ;; Check registered name
  (is (eq 'gtk:input-hints
          (glib:symbol-for-gtype "GtkInputHints")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInputHints")
          (g:gtype (cffi:foreign-funcall "gtk_input_hints_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_INPUT_HINT_NONE" "GTK_INPUT_HINT_SPELLCHECK"
               "GTK_INPUT_HINT_NO_SPELLCHECK" "GTK_INPUT_HINT_WORD_COMPLETION"
               "GTK_INPUT_HINT_LOWERCASE" "GTK_INPUT_HINT_UPPERCASE_CHARS"
               "GTK_INPUT_HINT_UPPERCASE_WORDS"
               "GTK_INPUT_HINT_UPPERCASE_SENTENCES"
               "GTK_INPUT_HINT_INHIBIT_OSK" "GTK_INPUT_HINT_VERTICAL_WRITING"
               "GTK_INPUT_HINT_EMOJI" "GTK_INPUT_HINT_NO_EMOJI")
             (glib-test:list-flags-item-names "GtkInputHints")))
  ;; Check values
  (is (equal '(0 1 2 4 8 16 32 64 128 256 512 1024)
             (glib-test:list-flags-item-values "GtkInputHints")))
  ;; Check nick names
  (is (equal '("none" "spellcheck" "no-spellcheck" "word-completion" "lowercase"
               "uppercase-chars" "uppercase-words" "uppercase-sentences"
               "inhibit-osk" "vertical-writing" "emoji" "no-emoji")
             (glib-test:list-flags-item-nicks "GtkInputHints")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkInputHints" GTK:INPUT-HINTS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_input_hints_get_type")
                       (:NONE 0)
                       (:SPELLCHECK 1)
                       (:NO-SPELLCHECK 2)
                       (:WORD-COMPLETION 4)
                       (:LOWERCASE 8)
                       (:UPPERCASE-CHARS 16)
                       (:UPPERCASE-WORDS 32)
                       (:UPPERCASE-SENTENCES 64)
                       (:INHIBIT-OSK 128)
                       (:VERTICAL-WRITING 256)
                       (:EMOJI 512)
                       (:NO-EMOJI 1024))
             (gobject:get-gtype-definition "GtkInputHints"))))

;;;     GtkEntry

(test gtk-entry-class
  ;; Check type
  (is (g:type-is-object "GtkEntry"))
  ;; Check registered name
  (is (eq 'gtk:entry
          (glib:symbol-for-gtype "GtkEntry")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEntry")
          (g:gtype (cffi:foreign-funcall "gtk_entry_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkEntry")))
  ;; Check children
  (is (or (equal '("GtkFileChooserEntry" "GtkSearchEntry" "GtkSpinButton")
                 (glib-test:list-children "GtkEntry"))
          (equal '("GtkSearchEntry" "GtkSpinButton")
                 (glib-test:list-children "GtkEntry"))))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkEditable"
               "GtkCellEditable")
             (glib-test:list-interfaces "GtkEntry")))
  ;; Check class properties
  (is (equal '("activates-default" "attributes" "buffer" "caps-lock-warning"
               "completion" "cursor-position" "editable" "editing-canceled"
               "enable-emoji-completion" "has-frame" "im-module" "inner-border"
               "input-hints" "input-purpose" "invisible-char"
               "invisible-char-set" "max-length" "max-width-chars"
               "overwrite-mode" "placeholder-text" "populate-all"
               "primary-icon-activatable" "primary-icon-gicon"
               "primary-icon-name" "primary-icon-pixbuf"
               "primary-icon-sensitive" "primary-icon-stock"
               "primary-icon-storage-type" "primary-icon-tooltip-markup"
               "primary-icon-tooltip-text" "progress-fraction"
               "progress-pulse-step" "scroll-offset"
               "secondary-icon-activatable" "secondary-icon-gicon"
               "secondary-icon-name" "secondary-icon-pixbuf"
               "secondary-icon-sensitive" "secondary-icon-stock"
               "secondary-icon-storage-type" "secondary-icon-tooltip-markup"
               "secondary-icon-tooltip-text" "selection-bound" "shadow-type"
               "show-emoji-icon" "tabs" "text" "text-length"
               "truncate-multiline" "visibility" "width-chars" "xalign")
             (glib-test:list-properties "GtkEntry")))
  ;; Check style properties
  (is (equal '("icon-prelight" "inner-border" "invisible-char"
               "progress-border")
             (gtk-test:list-style-properties "GtkEntry")))
  ;; Check signals
  ;; TODO: TOGGLE-OVERWRITE since Gtk 3.24.49 is not documented
  #-windows
  (is (equal '("activate" "backspace" "copy-clipboard" "cut-clipboard"
               "delete-from-cursor" "icon-press" "icon-release"
               "insert-at-cursor" "insert-emoji" "move-cursor" "paste-clipboard"
               "populate-popup" "preedit-changed" "toggle-overwrite")
             (glib-test:list-signals "GtkEntry")))
  #+windows ;; on Windows version 3.24.49 installed
  (is (equal '("activate" "backspace" "copy-clipboard" "cut-clipboard"
               "delete-from-cursor" "icon-press" "icon-release"
               "insert-at-cursor" "insert-emoji" "move-cursor" "paste-clipboard"
               "populate-popup" "preedit-changed" "toggle-direction"
               "toggle-overwrite")
             (glib-test:list-signals "GtkEntry")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEntry" GTK:ENTRY
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                         "GtkEditable")
                        :TYPE-INITIALIZER "gtk_entry_get_type")
                       ((ACTIVATES-DEFAULT ENTRY-ACTIVATES-DEFAULT
                         "activates-default" "gboolean" T T)
                        (ATTRIBUTES ENTRY-ATTRIBUTES
                         "attributes" "PangoAttrList" T T)
                        (BUFFER ENTRY-BUFFER "buffer" "GtkEntryBuffer" T T)
                        (CAPS-LOCK-WARNING ENTRY-CAPS-LOCK-WARNING
                         "caps-lock-warning" "gboolean" T T)
                        (COMPLETION ENTRY-COMPLETION
                         "completion" "GtkEntryCompletion" T T)
                        (CURSOR-POSITION ENTRY-CURSOR-POSITION
                         "cursor-position" "gint" T NIL)
                        (EDITABLE ENTRY-EDITABLE "editable" "gboolean" T T)
                        (ENABLE-EMOJI-COMPLETION ENTRY-ENABLE-EMOJI-COMPLETION
                         "enable-emoji-completion" "gboolean" T T)
                        (HAS-FRAME ENTRY-HAS-FRAME "has-frame" "gboolean" T T)
                        (IM-MODULE ENTRY-IM-MODULE "im-module" "gchararray" T T)
                        (INNER-BORDER ENTRY-INNER-BORDER
                         "inner-border" "GtkBorder" T T)
                        (INPUT-HINTS ENTRY-INPUT-HINTS
                         "input-hints" "GtkInputHints" T T)
                        (INPUT-PURPOSE ENTRY-INPUT-PURPOSE
                         "input-purpose" "GtkInputPurpose" T T)
                        (INVISIBLE-CHAR ENTRY-INVISIBLE-CHAR
                         "invisible-char" "guint" T T)
                        (INVISIBLE-CHAR-SET ENTRY-INVISIBLE-CHAR-SET
                         "invisible-char-set" "gboolean" T T)
                        (MAX-LENGTH ENTRY-MAX-LENGTH "max-length" "gint" T T)
                        (MAX-WIDTH-CHARS ENTRY-MAX-WIDTH-CHARS
                         "max-width-chars" "gint" T T)
                        (OVERWRITE-MODE ENTRY-OVERWRITE-MODE
                         "overwrite-mode" "gboolean" T T)
                        (PLACEHOLDER-TEXT ENTRY-PLACEHOLDER-TEXT
                         "placeholder-text" "gchararray" T T)
                        (POPULATE-ALL ENTRY-POPULATE-ALL
                         "populate-all" "gboolean" T T)
                        (PRIMARY-ICON-ACTIVATABLE ENTRY-PRIMARY-ICON-ACTIVATABLE
                         "primary-icon-activatable" "gboolean" T T)
                        (PRIMARY-ICON-GICON ENTRY-PRIMARY-ICON-GICON
                         "primary-icon-gicon" "GIcon" T T)
                        (PRIMARY-ICON-NAME ENTRY-PRIMARY-ICON-NAME
                         "primary-icon-name" "gchararray" T T)
                        (PRIMARY-ICON-PIXBUF ENTRY-PRIMARY-ICON-PIXBUF
                         "primary-icon-pixbuf" "GdkPixbuf" T T)
                        (PRIMARY-ICON-SENSITIVE ENTRY-PRIMARY-ICON-SENSITIVE
                         "primary-icon-sensitive" "gboolean" T T)
                        (PRIMARY-ICON-STOCK ENTRY-PRIMARY-ICON-STOCK
                         "primary-icon-stock" "gchararray" T T)
                        (PRIMARY-ICON-STORAGE-TYPE
                         ENTRY-PRIMARY-ICON-STORAGE-TYPE
                         "primary-icon-storage-type" "GtkImageType" T NIL)
                        (PRIMARY-ICON-TOOLTIP-MARKUP
                         ENTRY-PRIMARY-ICON-TOOLTIP-MARKUP
                         "primary-icon-tooltip-markup" "gchararray" T T)
                        (PRIMARY-ICON-TOOLTIP-TEXT
                         ENTRY-PRIMARY-ICON-TOOLTIP-TEXT
                         "primary-icon-tooltip-text" "gchararray" T T)
                        (PROGRESS-FRACTION ENTRY-PROGRESS-FRACTION
                         "progress-fraction" "gdouble" T T)
                        (PROGRESS-PULSE-STEP ENTRY-PROGRESS-PULSE-STEP
                         "progress-pulse-step" "gdouble" T T)
                        (SCROLL-OFFSET ENTRY-SCROLL-OFFSET
                         "scroll-offset" "gint" T NIL)
                        (SECONDARY-ICON-ACTIVATABLE
                         ENTRY-SECONDARY-ICON-ACTIVATABLE
                         "secondary-icon-activatable" "gboolean" T T)
                        (SECONDARY-ICON-GICON ENTRY-SECONDARY-ICON-GICON
                         "secondary-icon-gicon" "GIcon" T T)
                        (SECONDARY-ICON-NAME ENTRY-SECONDARY-ICON-NAME
                         "secondary-icon-name" "gchararray" T T)
                        (SECONDARY-ICON-PIXBUF ENTRY-SECONDARY-ICON-PIXBUF
                         "secondary-icon-pixbuf" "GdkPixbuf" T T)
                        (SECONDARY-ICON-SENSITIVE
                         ENTRY-SECONDARY-ICON-SENSITIVE
                         "secondary-icon-sensitive" "gboolean" T T)
                        (SECONDARY-ICON-STOCK ENTRY-SECONDARY-ICON-STOCK
                         "secondary-icon-stock" "gchararray" T T)
                        (SECONDARY-ICON-STORAGE-TYPE
                         ENTRY-SECONDARY-ICON-STORAGE-TYPE
                         "secondary-icon-storage-type" "GtkImageType" T NIL)
                        (SECONDARY-ICON-TOOLTIP-MARKUP
                         ENTRY-SECONDARY-ICON-TOOLTIP-MARKUP
                         "secondary-icon-tooltip-markup" "gchararray" T T)
                        (SECONDARY-ICON-TOOLTIP-TEXT
                         ENTRY-SECONDARY-ICON-TOOLTIP-TEXT
                         "secondary-icon-tooltip-text" "gchararray" T T)
                        (SELECTION-BOUND ENTRY-SELECTION-BOUND
                         "selection-bound" "gint" T NIL)
                        (SHADOW-TYPE ENTRY-SHADOW-TYPE
                         "shadow-type" "GtkShadowType" T T)
                        (SHOW-EMOJI-ICON ENTRY-SHOW-EMOJI-ICON
                         "show-emoji-icon" "gboolean" T T)
                        (TABS ENTRY-TABS "tabs" "PangoTabArray" T T)
                        (TEXT ENTRY-TEXT "text" "gchararray" T T)
                        (TEXT-LENGTH ENTRY-TEXT-LENGTH
                         "text-length" "guint" T NIL)
                        (TRUNCATE-MULTILINE ENTRY-TRUNCATE-MULTILINE
                         "truncate-multiline" "gboolean" T T)
                        (VISIBILITY ENTRY-VISIBILITY
                         "visibility" "gboolean" T T)
                        (WIDTH-CHARS ENTRY-WIDTH-CHARS "width-chars" "gint" T T)
                        (XALIGN ENTRY-XALIGN "xalign" "gfloat" T T)))
             (gobject:get-gtype-definition "GtkEntry"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-entry-properties
  (let ((entry (make-instance 'gtk:entry)))
    (is-false (gtk:entry-activates-default entry))
    (is-false (gtk:entry-attributes entry))
    (is (typep (gtk:entry-buffer entry) 'gtk:entry-buffer))
    (is-true (gtk:entry-caps-lock-warning entry))
    (is-false (gtk:entry-completion entry))
    (is (= 0 (gtk:entry-cursor-position entry)))
    (is-true (gtk:entry-editable entry))
    (is-false (gtk:entry-enable-emoji-completion entry))
    (is-true (gtk:entry-has-frame entry))
    (is-false (gtk:entry-im-module entry))
    (is-false (gtk:entry-inner-border entry))
    (is-false (gtk:entry-input-hints entry))
    (is (eq :free-form (gtk:entry-input-purpose entry)))
    #-windows
    (is (= 8226 (gtk:entry-invisible-char entry)))
    #+windows
    (is (= 9679 (gtk:entry-invisible-char entry)))
    (is-false (gtk:entry-invisible-char-set entry))
    (is (= 0 (gtk:entry-max-length entry)))
    (is (= -1 (gtk:entry-max-width-chars entry)))
    (is-false (gtk:entry-overwrite-mode entry))
    (is-false (gtk:entry-placeholder-text entry))
    (is-false (gtk:entry-populate-all entry))
    (is-true (gtk:entry-primary-icon-activatable entry))
    (is-false (gtk:entry-primary-icon-gicon entry))
    (is-false (gtk:entry-primary-icon-name entry))
    (is-false (gtk:entry-primary-icon-pixbuf entry))
    (is-true (gtk:entry-primary-icon-sensitive entry))
    (is-false (gtk:entry-primary-icon-stock entry))
    (is (eq :empty (gtk:entry-primary-icon-storage-type entry)))
    (is-false (gtk:entry-primary-icon-tooltip-markup entry))
    (is-false (gtk:entry-primary-icon-tooltip-text entry))
    (is (= 0.0d0 (gtk:entry-progress-fraction entry)))
    (is (= 0.1d0 (gtk:entry-progress-pulse-step entry)))
    (is (= 0 (gtk:entry-scroll-offset entry)))
    (is-true (gtk:entry-secondary-icon-activatable entry))
    (is-false (gtk:entry-secondary-icon-gicon entry))
    (is-false (gtk:entry-secondary-icon-name entry))
    (is-false (gtk:entry-secondary-icon-pixbuf entry))
    (is-true (gtk:entry-secondary-icon-sensitive entry))
    (is-false (gtk:entry-secondary-icon-stock entry))
    (is (eq :empty (gtk:entry-secondary-icon-storage-type entry)))
    (is-false (gtk:entry-secondary-icon-tooltip-markup entry))
    (is-false (gtk:entry-secondary-icon-tooltip-text entry))
    (is (= 0 (gtk:entry-selection-bound entry)))
    (is (eq :in (gtk:entry-shadow-type entry)))
    (is-false (gtk:entry-show-emoji-icon entry))
    (is-false (gtk:entry-tabs entry))
    (is (string= "" (gtk:entry-text entry)))
    (is (= 0 (gtk:entry-text-length entry)))
    (is-false (gtk:entry-truncate-multiline entry))
    (is-true (gtk:entry-visibility entry))
    (is (= -1 (gtk:entry-width-chars entry)))
    (is (= 0.0 (gtk:entry-xalign entry)))))

;;; --- Style Properties -------------------------------------------------------

;;;     icon-prelight
;;;     inner-border
;;;     invisible-char
;;;     progress-border

;;; --- Signals ----------------------------------------------------------------

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

;;; --- Functions --------------------------------------------------------------

;;;     gtk_entry_new
;;;     gtk_entry_new_with_buffer
;;;     gtk_entry_get_text_area
;;;     gtk_entry_unset_invisible_char
;;;     gtk_entry_set_alignment
;;;     gtk_entry_get_alignment
;;;     gtk_entry_get_layout
;;;     gtk_entry_get_layout_offsets
;;;     gtk_entry_layout_index_to_text_index
;;;     gtk_entry_text_index_to_layout_index
;;;     gtk_entry_set_cursor_hadjustment
;;;     gtk_entry_get_cursor_hadjustment
;;;     gtk_entry_progress_pulse
;;;     gtk_entry_im_context_filter_keypress
;;;     gtk_entry_reset_im_context
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
;;;     gtk_entry_grab_focus_without_selecting

;;; 2024-9-22
