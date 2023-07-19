(in-package :gtk-test)

(def-suite gtk-cell-renderer-text :in gtk-suite)
(in-suite gtk-cell-renderer-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererText

(test cell-renderer-text-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererText"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-text
          (glib:symbol-for-gtype "GtkCellRendererText")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererText")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_text_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererText")))
  ;; Check the children
  (is (equal '("GtkCellRendererAccel" "GtkCellRendererCombo"
               "GtkCellRendererSpin")
             (list-children "GtkCellRendererText")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererText")))
  ;; Check the class properties
  (is (equal '("align-set" "alignment" "attributes" "background"
               "background-gdk" "background-rgba" "background-set" "editable"
               "editable-set" "ellipsize" "ellipsize-set" "family" "family-set"
               "font" "font-desc" "foreground" "foreground-gdk"
               "foreground-rgba" "foreground-set" "language" "language-set"
               "markup" "max-width-chars" "placeholder-text" "rise" "rise-set"
               "scale" "scale-set" "single-paragraph-mode" "size" "size-points"
               "size-set" "stretch" "stretch-set" "strikethrough"
               "strikethrough-set" "style" "style-set" "text" "underline"
               "underline-set" "variant" "variant-set" "weight" "weight-set"
               "width-chars" "wrap-mode" "wrap-width")
             (list-properties "GtkCellRendererText")))
  ;; Check the signals
  (is (equal '("edited")
             (list-signals "GtkCellRendererText")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererText" 
                                             GTK-CELL-RENDERER-TEXT
                       (:SUPERCLASS GTK-CELL-RENDERER :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_cell_renderer_text_get_type")
                       ((ALIGN-SET GTK-CELL-RENDERER-TEXT-ALIGN-SET "align-set"
                         "gboolean" T T)
                        (ALIGNMENT GTK-CELL-RENDERER-TEXT-ALIGNMENT "alignment"
                         "PangoAlignment" T T)
                        (ATTRIBUTES GTK-CELL-RENDERER-TEXT-ATTRIBUTES
                         "attributes" "PangoAttrList" T T)
                        (BACKGROUND GTK-CELL-RENDERER-TEXT-BACKGROUND
                         "background" "gchararray" NIL T)
                        (BACKGROUND-GDK GTK-CELL-RENDERER-TEXT-BACKGROUND-GDK
                         "background-gdk" "GdkColor" T T)
                        (BACKGROUND-RGBA GTK-CELL-RENDERER-TEXT-BACKGROUND-RGBA
                         "background-rgba" "GdkRGBA" T T)
                        (BACKGROUND-SET GTK-CELL-RENDERER-TEXT-BACKGROUND-SET
                         "background-set" "gboolean" T T)
                        (EDITABLE GTK-CELL-RENDERER-TEXT-EDITABLE "editable"
                         "gboolean" T T)
                        (EDITABLE-SET GTK-CELL-RENDERER-TEXT-EDITABLE-SET
                         "editable-set" "gboolean" T T)
                        (ELLIPSIZE GTK-CELL-RENDERER-TEXT-ELLIPSIZE "ellipsize"
                         "PangoEllipsizeMode" T T)
                        (ELLIPSIZE-SET GTK-CELL-RENDERER-TEXT-ELLIPSIZE-SET
                         "ellipsize-set" "gboolean" T T)
                        (FAMILY GTK-CELL-RENDERER-TEXT-FAMILY "family"
                         "gchararray" T T)
                        (FAMILY-SET GTK-CELL-RENDERER-TEXT-FAMILY-SET
                         "family-set" "gboolean" T T)
                        (FONT GTK-CELL-RENDERER-TEXT-FONT "font" "gchararray" T
                         T)
                        (FONT-DESC GTK-CELL-RENDERER-TEXT-FONT-DESC "font-desc"
                         "PangoFontDescription" T T)
                        (FOREGROUND GTK-CELL-RENDERER-TEXT-FOREGROUND
                         "foreground" "gchararray" NIL T)
                        (FOREGROUND-GDK GTK-CELL-RENDERER-TEXT-FOREGROUND-GDK
                         "foreground-gdk" "GdkColor" T T)
                        (FOREGROUND-RGBA GTK-CELL-RENDERER-TEXT-FOREGROUND-RGBA
                         "foreground-rgba" "GdkRGBA" T T)
                        (FOREGROUND-SET GTK-CELL-RENDERER-TEXT-FOREGROUND-SET
                         "foreground-set" "gboolean" T T)
                        (LANGUAGE GTK-CELL-RENDERER-TEXT-LANGUAGE "language"
                         "gchararray" T T)
                        (LANGUAGE-SET GTK-CELL-RENDERER-TEXT-LANGUAGE-SET
                         "language-set" "gboolean" T T)
                        (MARKUP GTK-CELL-RENDERER-TEXT-MARKUP "markup"
                         "gchararray" NIL T)
                        (MAX-WIDTH-CHARS GTK-CELL-RENDERER-TEXT-MAX-WIDTH-CHARS
                         "max-width-chars" "gint" T T)
                        (PLACEHOLDER-TEXT
                         GTK-CELL-RENDERER-TEXT-PLACEHOLDER-TEXT
                         "placeholder-text" "gchararray" T T)
                        (RISE GTK-CELL-RENDERER-TEXT-RISE "rise" "gint" T T)
                        (RISE-SET GTK-CELL-RENDERER-TEXT-RISE-SET "rise-set"
                         "gboolean" T T)
                        (SCALE GTK-CELL-RENDERER-TEXT-SCALE "scale" "gdouble" T
                         T)
                        (SCALE-SET GTK-CELL-RENDERER-TEXT-SCALE-SET "scale-set"
                         "gboolean" T T)
                        (SINGLE-PARAGRAPH-MODE
                         GTK-CELL-RENDERER-TEXT-SINGLE-PARAGRAPH-MODE
                         "single-paragraph-mode" "gboolean" T T)
                        (SIZE GTK-CELL-RENDERER-TEXT-SIZE "size" "gint" T T)
                        (SIZE-POINTS GTK-CELL-RENDERER-TEXT-SIZE-POINTS
                         "size-points" "gdouble" T T)
                        (SIZE-SET GTK-CELL-RENDERER-TEXT-SIZE-SET "size-set"
                         "gboolean" T T)
                        (STRETCH GTK-CELL-RENDERER-TEXT-STRETCH "stretch"
                         "PangoStretch" T T)
                        (STRETCH-SET GTK-CELL-RENDERER-TEXT-STRETCH-SET
                         "stretch-set" "gboolean" T T)
                        (STRIKETHROUGH GTK-CELL-RENDERER-TEXT-STRIKETHROUGH
                         "strikethrough" "gboolean" T T)
                        (STRIKETHROUGH-SET
                         GTK-CELL-RENDERER-TEXT-STRIKETHROUGH-SET
                         "strikethrough-set" "gboolean" T T)
                        (STYLE GTK-CELL-RENDERER-TEXT-STYLE "style"
                         "PangoStyle" T T)
                        (STYLE-SET GTK-CELL-RENDERER-TEXT-STYLE-SET "style-set"
                         "gboolean" T T)
                        (TEXT GTK-CELL-RENDERER-TEXT-TEXT "text" "gchararray" T
                         T)
                        (UNDERLINE GTK-CELL-RENDERER-TEXT-UNDERLINE "underline"
                         "PangoUnderline" T T)
                        (UNDERLINE-SET GTK-CELL-RENDERER-TEXT-UNDERLINE-SET
                         "underline-set" "gboolean" T T)
                        (VARIANT GTK-CELL-RENDERER-TEXT-VARIANT "variant"
                         "PangoVariant" T T)
                        (VARIANT-SET GTK-CELL-RENDERER-TEXT-VARIANT-SET
                         "variant-set" "gboolean" T T)
                        (WEIGHT GTK-CELL-RENDERER-TEXT-WEIGHT "weight" "gint" T
                         T)
                        (WEIGHT-SET GTK-CELL-RENDERER-TEXT-WEIGHT-SET
                         "weight-set" "gboolean" T T)
                        (WIDTH-CHARS GTK-CELL-RENDERER-TEXT-WIDTH-CHARS
                         "width-chars" "gint" T T)
                        (WRAP-MODE GTK-CELL-RENDERER-TEXT-WRAP-MODE "wrap-mode"
                         "PangoWrapMode" T T)
                        (WRAP-WIDTH GTK-CELL-RENDERER-TEXT-WRAP-WIDTH
                         "wrap-width" "gint" T T)))
             (gobject:get-g-type-definition "GtkCellRendererText"))))

;;; --- Properties -------------------------------------------------------------

(test cell-renderer-text-properties
  (let ((cell (gtk:cell-renderer-text-new)))
    (is-false (gtk:cell-renderer-text-align-set cell))
    (is (eq :left (gtk:cell-renderer-text-alignment cell)))
    (is-false (gtk:cell-renderer-text-attributes cell))
    ;; background is not readable
    (signals (error) (gtk:cell-renderer-text-background cell))
    (is (typep (gtk:cell-renderer-text-background-gdk cell) 'gdk:color))
    (is (typep (gtk:cell-renderer-text-background-rgba cell) 'gdk:rgba))
    (is-false (gtk:cell-renderer-text-background-set cell))
    (is-false (gtk:cell-renderer-text-editable cell))
    (is-false (gtk:cell-renderer-text-editable-set cell))
    (is (eq :none (gtk:cell-renderer-text-ellipsize cell)))
    (is-false (gtk:cell-renderer-text-ellipsize-set cell))
    (is-false (gtk:cell-renderer-text-family cell))
    (is-false (gtk:cell-renderer-text-family-set cell))
    (is (string= "Normal" (gtk:cell-renderer-text-font cell)))
    (is (typep (gtk:cell-renderer-text-font-desc cell) 'pango:font-description))
    ;; foreground in not readable
    (signals (error) (gtk:cell-renderer-text-foreground cell))
    (is (typep (gtk:cell-renderer-text-foreground-gdk cell) 'gdk:color))
    (is (typep (gtk:cell-renderer-text-foreground-rgba cell) 'gdk:rgba))
    (is-false (gtk:cell-renderer-text-foreground-set cell))
    (is-false (gtk:cell-renderer-text-language cell))
    (is-false (gtk:cell-renderer-text-language-set cell))
    ;; markup is not readable
    (signals (error) (gtk:cell-renderer-text-markup cell))
    (is (= -1 (gtk:cell-renderer-text-max-width-chars cell)))
    (is-false (gtk:cell-renderer-text-placeholder-text cell))
    (is-false (gtk:cell-renderer-text-rise-set cell))
    (is (= 1.0d0 (gtk:cell-renderer-text-scale cell)))
    (is-false (gtk:cell-renderer-text-scale-set cell))
    (is-false (gtk:cell-renderer-text-single-paragraph-mode cell))
    (is (= 0 (gtk:cell-renderer-text-size cell)))
    (is (= 0.0d0 (gtk:cell-renderer-text-size-points cell)))
    (is-false (gtk:cell-renderer-text-size-set cell))
    (is (eq :normal (gtk:cell-renderer-text-stretch cell)))
    (is-false (gtk:cell-renderer-text-stretch-set cell))
    (is-false (gtk:cell-renderer-text-strikethrough cell))
    (is-false (gtk:cell-renderer-text-strikethrough-set cell))
    (is (eq :normal (gtk:cell-renderer-text-style cell)))
    (is-false (gtk:cell-renderer-text-style-set cell))
    (is-false (gtk:cell-renderer-text-text cell))
    (is (eq :none (gtk:cell-renderer-text-underline cell)))
    (is-false (gtk:cell-renderer-text-underline-set cell))
    (is (eq :normal (gtk:cell-renderer-text-variant cell)))
    (is-false (gtk:cell-renderer-text-variant-set cell))
    (is (= 400 (gtk:cell-renderer-text-weight cell)))
    (is-false (gtk:cell-renderer-text-weight-set cell))
    (is (= -1 (gtk:cell-renderer-text-width-chars cell)))
    (is (eq :char (gtk:cell-renderer-text-wrap-mode cell)))
    (is (= -1 (gtk:cell-renderer-text-wrap-width cell)))))

;;; --- Signals ----------------------------------------------------------------

;;;     edited

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_text_new

(test cell-renderer-text-new
  (is (typep (gtk:cell-renderer-text-new) 'gtk:cell-renderer-text)))

;;;     gtk_cell_renderer_text_set_fixed_height_from_font

;;; --- 2023-5-29 --------------------------------------------------------------
