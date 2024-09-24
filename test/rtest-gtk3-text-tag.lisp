(in-package :gtk-test)

(def-suite gtk-text-tag :in gtk-suite)
(in-suite gtk-text-tag)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWrapMode                                     <--- gtk.text-view.lisp

(test gtk-wrap-mode
  ;; Check type
  (is (g:type-is-enum "GtkWrapMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWrapMode")
          (g:gtype (cffi:foreign-funcall "gtk_wrap_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:wrap-mode
          (glib:symbol-for-gtype "GtkWrapMode")))
  ;; Check names
  (is (equal '("GTK_WRAP_NONE" "GTK_WRAP_CHAR" "GTK_WRAP_WORD"
               "GTK_WRAP_WORD_CHAR")
             (glib-test:list-enum-item-names "GtkWrapMode")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkWrapMode")))
  ;; Check nick names
  (is (equal '("none" "char" "word" "word-char")
             (glib-test:list-enum-item-nicks "GtkWrapMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkWrapMode" GTK:WRAP-MODE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_wrap_mode_get_type")
                       (:NONE 0)
                       (:CHAR 1)
                       (:WORD 2)
                       (:WORD-CHAR 3))
             (gobject:get-gtype-definition "GtkWrapMode"))))

;;;     GtkTextTag

(test gtk-text-tag-class
  ;; Check type
  (is (g:type-is-object "GtkTextTag"))
  ;; Check registered name
  (is (eq 'gtk:text-tag
          (glib:symbol-for-gtype "GtkTextTag")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextTag")
          (g:gtype (cffi:foreign-funcall "gtk_text_tag_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkTextTag")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTextTag")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTextTag")))
  ;; Check the class properties
  (is (equal '("accumulative-margin" "background" "background-full-height"
               "background-full-height-set" "background-gdk" "background-rgba"
               "background-set" "direction" "editable" "editable-set" "fallback"
               "fallback-set" "family" "family-set" "font" "font-desc"
               "font-features" "font-features-set" "foreground" "foreground-gdk"
               "foreground-rgba" "foreground-set" "indent" "indent-set"
               "invisible" "invisible-set" "justification" "justification-set"
               "language" "language-set" "left-margin" "left-margin-set"
               "letter-spacing" "letter-spacing-set" "name"
               "paragraph-background" "paragraph-background-gdk"
               "paragraph-background-rgba" "paragraph-background-set"
               "pixels-above-lines" "pixels-above-lines-set"
               "pixels-below-lines" "pixels-below-lines-set"
               "pixels-inside-wrap" "pixels-inside-wrap-set" "right-margin"
               "right-margin-set" "rise" "rise-set" "scale" "scale-set" "size"
               "size-points" "size-set" "stretch" "stretch-set" "strikethrough"
               "strikethrough-rgba" "strikethrough-rgba-set" "strikethrough-set"
               "style" "style-set" "tabs" "tabs-set" "underline"
               "underline-rgba" "underline-rgba-set" "underline-set" "variant"
               "variant-set" "weight" "weight-set" "wrap-mode" "wrap-mode-set")
             (glib-test:list-properties "GtkTextTag")))
  ;; Check signals
  (is (equal '("event")
             (glib-test:list-signals "GtkTextTag")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTextTag" GTK:TEXT-TAG
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_text_tag_get_type")
                       ((ACCUMULATIVE-MARGIN TEXT-TAG-ACCUMULATIVE-MARGIN
                         "accumulative-margin" "gboolean" T T)
                        (BACKGROUND TEXT-TAG-BACKGROUND
                         "background" "gchararray" NIL T)
                        (BACKGROUND-FULL-HEIGHT TEXT-TAG-BACKGROUND-FULL-HEIGHT
                         "background-full-height" "gboolean" T T)
                        (BACKGROUND-FULL-HEIGHT-SET
                         TEXT-TAG-BACKGROUND-FULL-HEIGHT-SET
                         "background-full-height-set" "gboolean" T T)
                        (BACKGROUND-GDK TEXT-TAG-BACKGROUND-GDK
                         "background-gdk" "GdkColor" T T)
                        (BACKGROUND-RGBA TEXT-TAG-BACKGROUND-RGBA
                         "background-rgba" "GdkRGBA" T T)
                        (BACKGROUND-SET TEXT-TAG-BACKGROUND-SET
                         "background-set" "gboolean" T T)
                        (DIRECTION TEXT-TAG-DIRECTION
                         "direction" "GtkTextDirection" T T)
                        (EDITABLE TEXT-TAG-EDITABLE "editable" "gboolean" T T)
                        (EDITABLE-SET TEXT-TAG-EDITABLE-SET
                         "editable-set" "gboolean" T T)
                        (FALLBACK TEXT-TAG-FALLBACK "fallback" "gboolean" T T)
                        (FALLBACK-SET TEXT-TAG-FALLBACK-SET
                         "fallback-set" "gboolean" T T)
                        (FAMILY TEXT-TAG-FAMILY "family" "gchararray" T T)
                        (FAMILY-SET TEXT-TAG-FAMILY-SET
                         "family-set" "gboolean" T T)
                        (FONT TEXT-TAG-FONT "font" "gchararray" T T)
                        (FONT-DESC TEXT-TAG-FONT-DESC
                         "font-desc" "PangoFontDescription" T T)
                        (FONT-FEATURES TEXT-TAG-FONT-FEATURES
                         "font-features" "gchararray" T T)
                        (FONT-FEATURES-SET TEXT-TAG-FONT-FEATURES-SET
                         "font-features-set" "gboolean" T T)
                        (FOREGROUND TEXT-TAG-FOREGROUND
                         "foreground" "gchararray" NIL T)
                        (FOREGROUND-GDK TEXT-TAG-FOREGROUND-GDK
                         "foreground-gdk" "GdkColor" T T)
                        (FOREGROUND-RGBA TEXT-TAG-FOREGROUND-RGBA
                         "foreground-rgba" "GdkRGBA" T T)
                        (FOREGROUND-SET TEXT-TAG-FOREGROUND-SET
                         "foreground-set" "gboolean" T T)
                        (INDENT TEXT-TAG-INDENT "indent" "gint" T T)
                        (INDENT-SET TEXT-TAG-INDENT-SET
                         "indent-set" "gboolean" T T)
                        (INVISIBLE TEXT-TAG-INVISIBLE
                         "invisible" "gboolean" T T)
                        (INVISIBLE-SET TEXT-TAG-INVISIBLE-SET
                         "invisible-set" "gboolean" T T)
                        (JUSTIFICATION TEXT-TAG-JUSTIFICATION
                         "justification" "GtkJustification" T T)
                        (JUSTIFICATION-SET TEXT-TAG-JUSTIFICATION-SET
                         "justification-set" "gboolean" T T)
                        (LANGUAGE TEXT-TAG-LANGUAGE
                         "language" "gchararray" T T)
                        (LANGUAGE-SET TEXT-TAG-LANGUAGE-SET
                         "language-set" "gboolean" T T)
                        (LEFT-MARGIN TEXT-TAG-LEFT-MARGIN
                         "left-margin" "gint" T T)
                        (LEFT-MARGIN-SET TEXT-TAG-LEFT-MARGIN-SET
                         "left-margin-set" "gboolean" T T)
                        (LETTER-SPACING TEXT-TAG-LETTER-SPACING
                         "letter-spacing" "gint" T T)
                        (LETTER-SPACING-SET TEXT-TAG-LETTER-SPACING-SET
                         "letter-spacing-set" "gboolean" T T)
                        (NAME TEXT-TAG-NAME "name" "gchararray" T NIL)
                        (PARAGRAPH-BACKGROUND TEXT-TAG-PARAGRAPH-BACKGROUND
                         "paragraph-background" "gchararray" NIL T)
                        (PARAGRAPH-BACKGROUND-GDK
                         TEXT-TAG-PARAGRAPH-BACKGROUND-GDK
                         "paragraph-background-gdk" "GdkColor" T T)
                        (PARAGRAPH-BACKGROUND-RGBA
                         TEXT-TAG-PARAGRAPH-BACKGROUND-RGBA
                         "paragraph-background-rgba" "GdkRGBA" T T)
                        (PARAGRAPH-BACKGROUND-SET
                         TEXT-TAG-PARAGRAPH-BACKGROUND-SET
                         "paragraph-background-set" "gboolean" T T)
                        (PIXELS-ABOVE-LINES TEXT-TAG-PIXELS-ABOVE-LINES
                         "pixels-above-lines" "gint" T T)
                        (PIXELS-ABOVE-LINES-SET
                         TEXT-TAG-PIXELS-ABOVE-LINES-SET
                         "pixels-above-lines-set" "gboolean" T T)
                        (PIXELS-BELOW-LINES TEXT-TAG-PIXELS-BELOW-LINES
                         "pixels-below-lines" "gint" T T)
                        (PIXELS-BELOW-LINES-SET TEXT-TAG-PIXELS-BELOW-LINES-SET
                         "pixels-below-lines-set" "gboolean" T T)
                        (PIXELS-INSIDE-WRAP TEXT-TAG-PIXELS-INSIDE-WRAP
                         "pixels-inside-wrap" "gint" T T)
                        (PIXELS-INSIDE-WRAP-SET TEXT-TAG-PIXELS-INSIDE-WRAP-SET
                         "pixels-inside-wrap-set" "gboolean" T T)
                        (RIGHT-MARGIN TEXT-TAG-RIGHT-MARGIN
                         "right-margin" "gint" T T)
                        (RIGHT-MARGIN-SET TEXT-TAG-RIGHT-MARGIN-SET
                         "right-margin-set" "gboolean" T T)
                        (RISE TEXT-TAG-RISE "rise" "gint" T T)
                        (RISE-SET TEXT-TAG-RISE-SET "rise-set" "gboolean" T T)
                        (SCALE TEXT-TAG-SCALE "scale" "gdouble" T T)
                        (SCALE-SET TEXT-TAG-SCALE-SET "scale-set" "gboolean" T T)
                        (SIZE TEXT-TAG-SIZE "size" "gint" T T)
                        (SIZE-POINTS TEXT-TAG-SIZE-POINTS
                         "size-points" "gdouble" T T)
                        (SIZE-SET TEXT-TAG-SIZE-SET "size-set" "gboolean" T T)
                        (STRETCH TEXT-TAG-STRETCH "stretch" "PangoStretch" T T)
                        (STRETCH-SET TEXT-TAG-STRETCH-SET
                         "stretch-set" "gboolean" T T)
                        (STRIKETHROUGH TEXT-TAG-STRIKETHROUGH
                         "strikethrough" "gboolean" T T)
                        (STRIKETHROUGH-RGBA TEXT-TAG-STRIKETHROUGH-RGBA
                         "strikethrough-rgba" "GdkRGBA" T T)
                        (STRIKETHROUGH-RGBA-SET
                         TEXT-TAG-STRIKETHROUGH-RGBA-SET
                         "strikethrough-rgba-set" "gboolean" T T)
                        (STRIKETHROUGH-SET TEXT-TAG-STRIKETHROUGH-SET
                         "strikethrough-set" "gboolean" T T)
                        (STYLE TEXT-TAG-STYLE "style" "PangoStyle" T T)
                        (STYLE-SET TEXT-TAG-STYLE-SET "style-set" "gboolean" T T)
                        (TABS TEXT-TAG-TABS "tabs" "PangoTabArray" T T)
                        (TABS-SET TEXT-TAG-TABS-SET "tabs-set" "gboolean" T T)
                        (UNDERLINE TEXT-TAG-UNDERLINE
                         "underline" "PangoUnderline" T T)
                        (UNDERLINE-RGBA TEXT-TAG-UNDERLINE-RGBA
                         "underline-rgba" "GdkRGBA" T T)
                        (UNDERLINE-RGBA-SET TEXT-TAG-UNDERLINE-RGBA-SET
                         "underline-rgba-set" "gboolean" T T)
                        (UNDERLINE-SET TEXT-TAG-UNDERLINE-SET
                         "underline-set" "gboolean" T T)
                        (VARIANT TEXT-TAG-VARIANT "variant" "PangoVariant" T T)
                        (VARIANT-SET TEXT-TAG-VARIANT-SET
                         "variant-set" "gboolean" T T)
                        (WEIGHT TEXT-TAG-WEIGHT "weight" "gint" T T)
                        (WEIGHT-SET TEXT-TAG-WEIGHT-SET
                         "weight-set" "gboolean" T T)
                        (WRAP-MODE TEXT-TAG-WRAP-MODE
                         "wrap-mode" "GtkWrapMode" T T)
                        (WRAP-MODE-SET TEXT-TAG-WRAP-MODE-SET
                         "wrap-mode-set" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkTextTag"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-text-tag-properties
  (let ((tag (make-instance 'gtk:text-tag)))
    (is-false (gtk:text-tag-accumulative-margin tag))
    ;; Property "background" is not writeable
    (signals (error) (gtk:text-tag-background tag))
    (is-false (gtk:text-tag-background-full-height tag))
    (is-false (gtk:text-tag-background-full-height-set tag))
    (is (eq 'gdk:color (type-of (gtk:text-tag-background-gdk tag))))
    (is-false (gtk:text-tag-background-rgba tag))
    (is-false (gtk:text-tag-background-set tag))
    (is (eq :none (gtk:text-tag-direction tag)))
    (is-true (gtk:text-tag-editable tag))
    (is-false (gtk:text-tag-editable-set tag))
    (is-true (gtk:text-tag-fallback tag))
    (is-false (gtk:text-tag-fallback-set tag))
    (is-false (gtk:text-tag-family tag))
    (is-false (gtk:text-tag-family-set tag))
    (is (string= "Normal" (gtk:text-tag-font tag)))
    (is (eq 'pango:font-description (type-of (gtk:text-tag-font-desc tag))))
    (is-false (gtk:text-tag-font-features tag))
    (is-false (gtk:text-tag-font-features-set tag))
    ;; Property "foreground" is not readable
    (signals (error) (gtk:text-tag-foreground tag))
    (is (eq 'gdk:color (type-of (gtk:text-tag-foreground-gdk tag))))
    (is-false (gtk:text-tag-foreground-rgba tag))
    (is-false (gtk:text-tag-foreground-set tag))
    (is (= 0 (gtk:text-tag-indent tag)))
    (is-false (gtk:text-tag-indent-set tag))
    (is-false (gtk:text-tag-invisible tag))
    (is-false (gtk:text-tag-invisible-set tag))
    (is (eq :left (gtk:text-tag-justification tag)))
    (is-false (gtk:text-tag-justification-set tag))
    (is (string= "de-de" (gtk:text-tag-language tag)))
    (is-false (gtk:text-tag-language-set tag))
    (is (= 0 (gtk:text-tag-left-margin tag)))
    (is-false (gtk:text-tag-left-margin-set tag))
    (is (= 0 (gtk:text-tag-letter-spacing tag)))
    (is-false (gtk:text-tag-letter-spacing-set tag))
    (is-false (gtk:text-tag-name tag))
    (signals (error) (gtk:text-tag-paragraph-background tag))
    (is-false (gtk:text-tag-paragraph-background-gdk tag))
    (is-false (gtk:text-tag-paragraph-background-rgba tag))
    (is-false (gtk:text-tag-paragraph-background-set tag))
    (is (= 0 (gtk:text-tag-pixels-above-lines tag)))
    (is-false (gtk:text-tag-pixels-above-lines-set tag))
    (is (= 0 (gtk:text-tag-pixels-below-lines tag)))
    (is-false (gtk:text-tag-pixels-below-lines-set tag))
    (is (= 0 (gtk:text-tag-pixels-inside-wrap tag)))
    (is-false (gtk:text-tag-pixels-inside-wrap-set tag))
    (is (= 0 (gtk:text-tag-right-margin tag)))
    (is-false (gtk:text-tag-right-margin-set tag))
    (is (= 0 (gtk:text-tag-rise tag)))
    (is-false (gtk:text-tag-rise-set tag))
    (is (= 1.0d0 (gtk:text-tag-scale tag)))
    (is-false (gtk:text-tag-scale-set tag))
    (is (= 0 (gtk:text-tag-size tag)))
    (is (= 0.0d0 (gtk:text-tag-size-points tag)))
    (is-false (gtk:text-tag-size-set tag))
    (is (eq :normal (gtk:text-tag-stretch tag)))
    (is-false (gtk:text-tag-stretch-set tag))
    (is-false (gtk:text-tag-strikethrough tag))
    (is-false (gtk:text-tag-strikethrough-rgba tag))
    (is-false (gtk:text-tag-strikethrough-rgba-set tag))
    (is-false (gtk:text-tag-strikethrough-set tag))
    (is (eq :normal (gtk:text-tag-style tag)))
    (is-false (gtk:text-tag-style-set tag))
    (is-false (gtk:text-tag-tabs tag))
    (is-false (gtk:text-tag-tabs-set tag))
    (is (eq :none (gtk:text-tag-underline tag)))
    (is-false (gtk:text-tag-underline-rgba tag))
    (is-false (gtk:text-tag-underline-rgba-set tag))
    (is-false (gtk:text-tag-underline-set tag))
    (is (eq :normal (gtk:text-tag-variant tag)))
    (is-false (gtk:text-tag-variant-set tag))
    (is (= 400 (gtk:text-tag-weight tag)))
    (is-false (gtk:text-tag-weight-set tag))
    (is (eq :none (gtk:text-tag-wrap-mode tag)))
    (is-false (gtk:text-tag-wrap-mode-set tag))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_tag_new

(test gtk-text-tag-new
  (is (typep (gtk:text-tag-new "tag") 'gtk:text-tag))
  (is (typep (gtk:text-tag-new "bold" :weight 700) 'gtk:text-tag))
  (is (typep (gtk:text-tag-new "blue-foreground" :foreground "blue")
             'gtk:text-tag))
  (is (typep (gtk:text-tag-new "italic" :style :italic) 'gtk:text-tag))
  (is (typep (gtk:text-tag-new "font" :font "fixed") 'gtk:text-tag))
  (is (typep (gtk:text-tag-new "font-italic" :font "fixed" :style :italic)
             'gtk:text-tag)))

;;;     gtk_text_tag_get_priority
;;;     gtk_text_tag_set_priority

(test gtk-text-tag-priority
  (let ((tag1 (gtk:text-tag-new "bold" :weight 700))
        (tag2 (gtk:text-tag-new "font" :font "fixed"))
        (table (gtk:text-tag-table-new)))

    (is-true (gtk:text-tag-table-add table tag1))
    (is (= 1 (gtk:text-tag-table-size table)))
    (is (= 0 (gtk:text-tag-priority tag1)))

    (is-true (gtk:text-tag-table-add table tag2))
    (is (= 2 (gtk:text-tag-table-size table)))
    (is (= 1 (gtk:text-tag-priority tag2)))

    (is (= 0 (setf (gtk:text-tag-priority tag2) 0)))
    (is (= 0 (gtk:text-tag-priority tag2)))
    (is (= 1 (setf (gtk:text-tag-priority tag2) 1)))
    (is (= 1 (gtk:text-tag-priority tag2)))))

;;;     gtk_text_tag_event
;;;     gtk_text_tag_changed

#+nil
(test gtk-text-tag-signals
  (let* ((tag (gtk:text-tag-new "bold"))
         (buffer (make-instance 'gtk:text-buffer :text "Some text."))
         (table (gtk:text-buffer-tag-table buffer))
         (view (gtk:text-view-new-with-buffer buffer))
         (iter (gtk:text-buffer-start-iter buffer))
         (result nil))
    (g-signal-connect tag "event"
                      (lambda (tag object event iter)
                        (setf result (cons "event" result))
                        (is (eq 'gtk:text-tag (type-of tag)))
                        (is (eq 'gtk:text-view (type-of object)))
                        (is (eq 'gdk:event (type-of event)))
                        (is (eq 'gtk:text-iter (type-of iter)))))
    (g-signal-connect table "tag-changed"
                      (lambda (table tag size-changed)
                        (setf result (cons "changed" result))
                        (is (eq 'gtk:text-tag-table (type-of table)))
                        (is (eq 'gtk:text-tag (type-of tag)))
                        (is (eq 'boolean (type-of size-changed)))))
    (is (eq 'gtk:text-tag (type-of tag)))
    (is (eq 'gtk:text-buffer (type-of buffer)))
    (is (eq 'gtk:text-tag-table (type-of table)))
    (is (eq 'gtk:text-view (type-of view)))
    (is (eq 'gtk:text-iter (type-of iter)))

    (gtk:text-tag-event tag view (make-event :type :nothing) iter)
    (gtk:text-tag-table-add table tag)
    (gtk:text-tag-changed tag 0)

    (is (equal '("changed" "event") result))))

;;;     gtk_text_attributes_new
;;;     gtk_text_attributes_copy
;;;     gtk_text_attributes_copy_values
;;;     gtk_text_attributes_unref
;;;     gtk_text_attributes_ref

;;; 2024-9-21
