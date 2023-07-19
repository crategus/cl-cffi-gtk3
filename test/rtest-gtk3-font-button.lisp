(in-package :gtk-test)

(def-suite gtk-font-button :in gtk-suite)
(in-suite gtk-font-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontButton

(test gtk-font-button-class
  ;; Type check
  (is (g:type-is-object "GtkFontButton"))
  ;; Check the registered name
  (is (eq 'gtk:font-button
          (glib:symbol-for-gtype "GtkFontButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontButton")
          (g:gtype (cffi:foreign-funcall "gtk_font_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkFontButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFontButton")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable" "GtkFontChooser")
             (list-interfaces "GtkFontButton")))
  ;; Check the class properties
  (is (equal '("font" "font-desc" "font-features" "font-name" "language" "level"
               "preview-text" "show-preview-entry" "show-size" "show-style"
               "title" "use-font" "use-size")
             (list-properties "GtkFontButton")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkFontButton")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkFontButton")))
  ;; Check the signals
  (is (equal '("font-set")
             (list-signals "GtkFontButton")))
  ;; CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkFontButton")))
  #-windows
  (is (string=
"[button.font:dir(ltr)]
  box.horizontal:dir(ltr)
    label:dir(ltr)
    box.horizontal:dir(ltr)
      separator.vertical:dir(ltr)
      label:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:font-button))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFontButton" GTK-FONT-BUTTON
                       (:SUPERCLASS GTK-BUTTON :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable" "GtkFontChooser")
                        :TYPE-INITIALIZER "gtk_font_button_get_type")
                       ((FONT-NAME GTK-FONT-BUTTON-FONT-NAME "font-name"
                         "gchararray" T T)
                        (SHOW-SIZE GTK-FONT-BUTTON-SHOW-SIZE "show-size"
                         "gboolean" T T)
                        (SHOW-STYLE GTK-FONT-BUTTON-SHOW-STYLE "show-style"
                         "gboolean" T T)
                        (TITLE GTK-FONT-BUTTON-TITLE "title" "gchararray" T T)
                        (USE-FONT GTK-FONT-BUTTON-USE-FONT "use-font"
                         "gboolean" T T)
                        (USE-SIZE GTK-FONT-BUTTON-USE-SIZE "use-size"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkFontButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     font-name
;;;     show-size
;;;     show-style
;;;     title
;;;     use-font
;;;     use-size

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

;;; --- 2023-6-16 --------------------------------------------------------------
