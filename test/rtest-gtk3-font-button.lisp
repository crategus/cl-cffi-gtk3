(in-package :gtk-test)

(def-suite gtk-font-button :in gtk-suite)
(in-suite gtk-font-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontButton

(test gtk-font-button-class
  ;; Check type
  (is (g:type-is-object "GtkFontButton"))
  ;; Check registered name
  (is (eq 'gtk:font-button
          (glib:symbol-for-gtype "GtkFontButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontButton")
          (g:gtype (cffi:foreign-funcall "gtk_font_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkFontButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFontButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable" "GtkFontChooser")
             (glib-test:list-interfaces "GtkFontButton")))
  ;; Check class properties
  (is (equal '("font" "font-desc" "font-features" "font-name" "language" "level"
               "preview-text" "show-preview-entry" "show-size" "show-style"
               "title" "use-font" "use-size")
             (glib-test:list-properties "GtkFontButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkFontButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkFontButton")))
  ;; Check signals
  (is (equal '("font-set")
             (glib-test:list-signals "GtkFontButton")))
  ;; CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkFontButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFontButton" GTK:FONT-BUTTON
                       (:SUPERCLASS GTK:BUTTON
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable" "GtkFontChooser")
                        :TYPE-INITIALIZER "gtk_font_button_get_type")
                       ((FONT-NAME FONT-BUTTON-FONT-NAME
                         "font-name" "gchararray" T T)
                        (SHOW-SIZE FONT-BUTTON-SHOW-SIZE
                         "show-size" "gboolean" T T)
                        (SHOW-STYLE FONT-BUTTON-SHOW-STYLE
                         "show-style" "gboolean" T T)
                        (TITLE FONT-BUTTON-TITLE "title" "gchararray" T T)
                        (USE-FONT FONT-BUTTON-USE-FONT "use-font" "gboolean" T T)
                        (USE-SIZE FONT-BUTTON-USE-SIZE
                         "use-size" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkFontButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-font-button-properties
  (let ((button (make-instance 'gtk:font-button)))
    (is (string= "Sans 12" (gtk:font-button-font-name button)))
    (is-true (gtk:font-button-show-size button))
    (is-true (gtk:font-button-show-style button))
    (is (string= "Wählen Sie eine Schrift" (gtk:font-button-title button)))
    (is-false (gtk:font-button-use-font button))
    (is-false (gtk:font-button-use-size button))))

;;; --- Signals ----------------------------------------------------------------

;;;     font-set

(test gtk-font-button-font-set-signal
  (let ((query (g:signal-query (g:signal-lookup "font-set" "GtkFontButton"))))
    (is (string= "font-set" (g:signal-query-signal-name query)))
    (is (string= "GtkFontButton"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_button_new

(test gtk-font-button-new
  (is (typep (gtk:font-button-new) 'gtk:font-button)))

;;;     gtk_font_button_new_with_font

(test gtk-font-button-new-with-font
  (let ((button (gtk:font-button-new-with-font "Courier")))
    (is (typep button 'gtk:font-button))
    (is (string= "Courier" (gtk:font-button-font-name button)))))

;;; 2024-9-23
