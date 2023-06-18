(in-package :gtk-test)

(def-suite gtk-font-chooser :in gtk-suite)
(in-suite gtk-font-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserLevel

(test gtk-font-chooser-level
  ;; Check the type
  (is (g:type-is-flags "GtkFontChooserLevel"))
  ;; Check the registered name
  (is (eq 'gtk:font-chooser-level
          (glib:symbol-for-gtype "GtkFontChooserLevel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontChooserLevel")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_level_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_FONT_CHOOSER_LEVEL_FAMILY" "GTK_FONT_CHOOSER_LEVEL_STYLE"
               "GTK_FONT_CHOOSER_LEVEL_SIZE" "GTK_FONT_CHOOSER_LEVEL_VARIATIONS"
               "GTK_FONT_CHOOSER_LEVEL_FEATURES")
             (list-flags-item-name "GtkFontChooserLevel")))
  ;; Check the values
  (is (equal '(0 1 2 4 8)
             (list-flags-item-value "GtkFontChooserLevel")))
  ;; Check the nick names
  (is (equal '("family" "style" "size" "variations" "features")
             (list-flags-item-nick "GtkFontChooserLevel")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkFontChooserLevel"
                              GTK-FONT-CHOOSER-LEVEL
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gtk_font_chooser_level_get_type")
                              (:FAMILY 0)
                              (:STYLE 1)
                              (:SIZE 2)
                              (:VARIATIONS 4)
                              (:FEATURES 8))
             (gobject:get-g-type-definition "GtkFontChooserLevel"))))

;;;     GtkFontChooser

(test gtk-font-chooser-interface
  ;; Type check
  (is (g:type-is-interface "GtkFontChooser"))
  ;; Check the registered name
  (is (eq 'gtk:font-chooser
          (glib:symbol-for-gtype "GtkFontChooser")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontChooser")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry")
             (list-interface-properties "GtkFontChooser")))
  ;; Check the signals
  (is (equal '("font-activated")
             (list-signals "GtkFontChooser")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkFontChooser" GTK-FONT-CHOOSER
                    (:EXPORT T :TYPE-INITIALIZER "gtk_font_chooser_get_type")
                    (FONT GTK-FONT-CHOOSER-FONT "font" "gchararray" T T)
                    (FONT-DESC GTK-FONT-CHOOSER-FONT-DESC "font-desc"
                     "PangoFontDescription" T T)
                    (FONT-FEATURES GTK-FONT-CHOOSER-FONT-FEATURES
                     "font-features" "gchararray" T NIL)
                    (LANGUAGE GTK-FONT-CHOOSER-LANGUAGE "language" "gchararray"
                     T T)
                    (LEVEL GTK-FONT-CHOOSER-LEVEL "level" "GtkFontChooserLevel"
                     T T)
                    (PREVIEW-TEXT GTK-FONT-CHOOSER-PREVIEW-TEXT "preview-text"
                     "gchararray" T T)
                    (SHOW-PREVIEW-ENTRY GTK-FONT-CHOOSER-SHOW-PREVIEW-ENTRY
                     "show-preview-entry" "gboolean" T T))
             (gobject:get-g-type-definition "GtkFontChooser"))))

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

;;; --- 2023-6-17 --------------------------------------------------------------
