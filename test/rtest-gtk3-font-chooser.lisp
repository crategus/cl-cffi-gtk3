(in-package :gtk-test)

(def-suite gtk-font-chooser :in gtk-suite)
(in-suite gtk-font-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserLevel

(test gtk-font-chooser-level
  ;; Check type
  (is (g:type-is-flags "GtkFontChooserLevel"))
  ;; Check registered name
  (is (eq 'gtk:font-chooser-level
          (glib:symbol-for-gtype "GtkFontChooserLevel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontChooserLevel")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_level_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_FONT_CHOOSER_LEVEL_FAMILY" "GTK_FONT_CHOOSER_LEVEL_STYLE"
               "GTK_FONT_CHOOSER_LEVEL_SIZE" "GTK_FONT_CHOOSER_LEVEL_VARIATIONS"
               "GTK_FONT_CHOOSER_LEVEL_FEATURES")
             (glib-test:list-flags-item-names "GtkFontChooserLevel")))
  ;; Check values
  (is (equal '(0 1 2 4 8)
             (glib-test:list-flags-item-values "GtkFontChooserLevel")))
  ;; Check nick names
  (is (equal '("family" "style" "size" "variations" "features")
             (glib-test:list-flags-item-nicks "GtkFontChooserLevel")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkFontChooserLevel"
                                     GTK:FONT-CHOOSER-LEVEL
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_font_chooser_level_get_type")
                       (:FAMILY 0)
                       (:STYLE 1)
                       (:SIZE 2)
                       (:VARIATIONS 4)
                       (:FEATURES 8))
             (gobject:get-gtype-definition "GtkFontChooserLevel"))))

;;;     GtkFontChooser

(test gtk-font-chooser-interface
  ;; Check type
  (is (g:type-is-interface "GtkFontChooser"))
  ;; Check registered name
  (is (eq 'gtk:font-chooser
          (glib:symbol-for-gtype "GtkFontChooser")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontChooser")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_get_type" :size))))
  ;; Check interface properties.
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry")
             (glib-test:list-interface-properties "GtkFontChooser")))
  ;; Check signals
  (is (equal '("font-activated")
             (glib-test:list-signals "GtkFontChooser")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkFontChooser" GTK:FONT-CHOOSER
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_font_chooser_get_type")
                       (FONT FONT-CHOOSER-FONT "font" "gchararray" T T)
                       (FONT-DESC FONT-CHOOSER-FONT-DESC
                        "font-desc" "PangoFontDescription" T T)
                       (FONT-FEATURES FONT-CHOOSER-FONT-FEATURES
                        "font-features" "gchararray" T NIL)
                       (LANGUAGE FONT-CHOOSER-LANGUAGE
                        "language" "gchararray" T T)
                       (LEVEL FONT-CHOOSER-LEVEL
                        "level" "GtkFontChooserLevel" T T)
                       (PREVIEW-TEXT FONT-CHOOSER-PREVIEW-TEXT
                        "preview-text" "gchararray" T T)
                       (SHOW-PREVIEW-ENTRY FONT-CHOOSER-SHOW-PREVIEW-ENTRY
                        "show-preview-entry" "gboolean" T T))
             (gobject:get-gtype-definition "GtkFontChooser"))))

;;; --- Properties -------------------------------------------------------------

;;;     font
;;;     font-desc
;;;     font-features
;;;     language
;;;     level
;;;     preview-text
;;;     show-preview-entry

(test gtk-font-chooser-properties
  (let ((button (gtk:font-button-new)))
    (is (string= "Sans 12" (gtk:font-chooser-font button)))
    (is (typep (gtk:font-chooser-font-desc button) 'pango:font-description))
    (is-false (gtk:font-chooser-font-features button))
    (is (string= "de-de" (gtk:font-chooser-language button)))
    (is (equal '(:style :size) (gtk:font-chooser-level button)))
    (is-false (gtk:font-chooser-preview-text button))
    (is-true (gtk:font-chooser-show-preview-entry button))))

;;; --- Signals ----------------------------------------------------------------

;;;     font-activated

(test gtk-font-chooser-font-activated-signal
  (let ((query (g:signal-query (g:signal-lookup "font-activated"
                                                "GtkFontChooser"))))
    (is (string= "font-activated" (g:signal-query-signal-name query)))
    (is (string= "GtkFontChooser"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gchararray")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_get_font_family

(test gtk-font-chooser-font-family
  (let ((button (gtk:font-button-new)))
    (is (typep (gtk:font-chooser-font-family button) 'pango:font-family))))

;;;     gtk_font_chooser_get_font_face

(test gtk-font-chooser-font-face
  (let ((button (gtk:font-button-new)))
    (is (typep (gtk:font-chooser-font-face button) 'pango:font-face))))

;;;     gtk_font_chooser_get_font_size

(test gtk-font-chooser-font-size
  (let ((button (gtk:font-button-new)))
    (is (= 12 (gtk:font-chooser-font-size button)))))

;;;     gtk_font_chooser_set_filter_func

;;;     gtk_font_chooser_set_font_map
;;;     gtk_font_chooser_get_font_map

(test gtk-font-chooser-font-map
  (let ((button (gtk:font-button-new)))
    (is-false (gtk:font-chooser-font-map button))))

;;; 2024-9-22
