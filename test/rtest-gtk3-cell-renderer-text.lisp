(in-package :gtk-test)

(def-suite gtk-cell-renderer-text :in gtk-suite)
(in-suite gtk-cell-renderer-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererText

(test gtk-cell-renderer-text-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererText"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-text
          (glib:symbol-for-gtype "GtkCellRendererText")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererText")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_text_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererText")))
  ;; Check children
  (is (equal '("GtkCellRendererAccel" "GtkCellRendererCombo"
               "GtkCellRendererSpin")
             (glib-test:list-children "GtkCellRendererText")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererText")))
  ;; Check class properties
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
             (glib-test:list-properties "GtkCellRendererText")))
  ;; Check signals
  (is (equal '("edited")
             (glib-test:list-signals "GtkCellRendererText")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererText"
                                      GTK:CELL-RENDERER-TEXT
                       (:SUPERCLASS GTK:CELL-RENDERER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_cell_renderer_text_get_type")
                       ((ALIGN-SET CELL-RENDERER-TEXT-ALIGN-SET "align-set"
                         "gboolean" T T)
                        (ALIGNMENT CELL-RENDERER-TEXT-ALIGNMENT "alignment"
                         "PangoAlignment" T T)
                        (ATTRIBUTES CELL-RENDERER-TEXT-ATTRIBUTES
                         "attributes" "PangoAttrList" T T)
                        (BACKGROUND CELL-RENDERER-TEXT-BACKGROUND
                         "background" "gchararray" NIL T)
                        (BACKGROUND-GDK CELL-RENDERER-TEXT-BACKGROUND-GDK
                         "background-gdk" "GdkColor" T T)
                        (BACKGROUND-RGBA CELL-RENDERER-TEXT-BACKGROUND-RGBA
                         "background-rgba" "GdkRGBA" T T)
                        (BACKGROUND-SET CELL-RENDERER-TEXT-BACKGROUND-SET
                         "background-set" "gboolean" T T)
                        (EDITABLE CELL-RENDERER-TEXT-EDITABLE "editable"
                         "gboolean" T T)
                        (EDITABLE-SET CELL-RENDERER-TEXT-EDITABLE-SET
                         "editable-set" "gboolean" T T)
                        (ELLIPSIZE CELL-RENDERER-TEXT-ELLIPSIZE "ellipsize"
                         "PangoEllipsizeMode" T T)
                        (ELLIPSIZE-SET CELL-RENDERER-TEXT-ELLIPSIZE-SET
                         "ellipsize-set" "gboolean" T T)
                        (FAMILY CELL-RENDERER-TEXT-FAMILY "family"
                         "gchararray" T T)
                        (FAMILY-SET CELL-RENDERER-TEXT-FAMILY-SET
                         "family-set" "gboolean" T T)
                        (FONT CELL-RENDERER-TEXT-FONT "font" "gchararray" T T)
                        (FONT-DESC CELL-RENDERER-TEXT-FONT-DESC "font-desc"
                         "PangoFontDescription" T T)
                        (FOREGROUND CELL-RENDERER-TEXT-FOREGROUND
                         "foreground" "gchararray" NIL T)
                        (FOREGROUND-GDK CELL-RENDERER-TEXT-FOREGROUND-GDK
                         "foreground-gdk" "GdkColor" T T)
                        (FOREGROUND-RGBA CELL-RENDERER-TEXT-FOREGROUND-RGBA
                         "foreground-rgba" "GdkRGBA" T T)
                        (FOREGROUND-SET CELL-RENDERER-TEXT-FOREGROUND-SET
                         "foreground-set" "gboolean" T T)
                        (LANGUAGE CELL-RENDERER-TEXT-LANGUAGE "language"
                         "gchararray" T T)
                        (LANGUAGE-SET CELL-RENDERER-TEXT-LANGUAGE-SET
                         "language-set" "gboolean" T T)
                        (MARKUP CELL-RENDERER-TEXT-MARKUP "markup"
                         "gchararray" NIL T)
                        (MAX-WIDTH-CHARS CELL-RENDERER-TEXT-MAX-WIDTH-CHARS
                         "max-width-chars" "gint" T T)
                        (PLACEHOLDER-TEXT CELL-RENDERER-TEXT-PLACEHOLDER-TEXT
                         "placeholder-text" "gchararray" T T)
                        (RISE CELL-RENDERER-TEXT-RISE "rise" "gint" T T)
                        (RISE-SET CELL-RENDERER-TEXT-RISE-SET "rise-set"
                         "gboolean" T T)
                        (SCALE CELL-RENDERER-TEXT-SCALE "scale" "gdouble" T T)
                        (SCALE-SET CELL-RENDERER-TEXT-SCALE-SET "scale-set"
                         "gboolean" T T)
                        (SINGLE-PARAGRAPH-MODE
                         CELL-RENDERER-TEXT-SINGLE-PARAGRAPH-MODE
                         "single-paragraph-mode" "gboolean" T T)
                        (SIZE CELL-RENDERER-TEXT-SIZE "size" "gint" T T)
                        (SIZE-POINTS CELL-RENDERER-TEXT-SIZE-POINTS
                         "size-points" "gdouble" T T)
                        (SIZE-SET CELL-RENDERER-TEXT-SIZE-SET "size-set"
                         "gboolean" T T)
                        (STRETCH CELL-RENDERER-TEXT-STRETCH "stretch"
                         "PangoStretch" T T)
                        (STRETCH-SET CELL-RENDERER-TEXT-STRETCH-SET
                         "stretch-set" "gboolean" T T)
                        (STRIKETHROUGH CELL-RENDERER-TEXT-STRIKETHROUGH
                         "strikethrough" "gboolean" T T)
                        (STRIKETHROUGH-SET CELL-RENDERER-TEXT-STRIKETHROUGH-SET
                         "strikethrough-set" "gboolean" T T)
                        (STYLE CELL-RENDERER-TEXT-STYLE "style" "PangoStyle" T T)
                        (STYLE-SET CELL-RENDERER-TEXT-STYLE-SET "style-set"
                         "gboolean" T T)
                        (TEXT CELL-RENDERER-TEXT-TEXT "text" "gchararray" T T)
                        (UNDERLINE CELL-RENDERER-TEXT-UNDERLINE "underline"
                         "PangoUnderline" T T)
                        (UNDERLINE-SET CELL-RENDERER-TEXT-UNDERLINE-SET
                         "underline-set" "gboolean" T T)
                        (VARIANT CELL-RENDERER-TEXT-VARIANT "variant"
                         "PangoVariant" T T)
                        (VARIANT-SET CELL-RENDERER-TEXT-VARIANT-SET
                         "variant-set" "gboolean" T T)
                        (WEIGHT CELL-RENDERER-TEXT-WEIGHT "weight" "gint" T T)
                        (WEIGHT-SET CELL-RENDERER-TEXT-WEIGHT-SET
                         "weight-set" "gboolean" T T)
                        (WIDTH-CHARS CELL-RENDERER-TEXT-WIDTH-CHARS
                         "width-chars" "gint" T T)
                        (WRAP-MODE CELL-RENDERER-TEXT-WRAP-MODE "wrap-mode"
                         "PangoWrapMode" T T)
                        (WRAP-WIDTH CELL-RENDERER-TEXT-WRAP-WIDTH
                         "wrap-width" "gint" T T)))
             (gobject:get-gtype-definition "GtkCellRendererText"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-text-properties
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

(test gtk-cell-renderer-text-new
  (is (typep (gtk:cell-renderer-text-new) 'gtk:cell-renderer-text)))

;;;     gtk_cell_renderer_text_set_fixed_height_from_font

;;; 2024-3-17
