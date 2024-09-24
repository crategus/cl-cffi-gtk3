(in-package :gtk-test)

(def-suite gtk-font-chooser-widget :in gtk-suite)
(in-suite gtk-font-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserWidget

(test gtk-font-chooser-widget-class
  ;; Check type
  (is (g:type-is-object "GtkFontChooserWidget"))
  ;; Check registered name
  (is (eq 'gtk:font-chooser-widget
          (glib:symbol-for-gtype "GtkFontChooserWidget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_widget_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkFontChooserWidget")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFontChooserWidget")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkFontChooser")
             (glib-test:list-interfaces "GtkFontChooserWidget")))
  ;; Check class properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry" "tweak-action")
             (glib-test:list-properties "GtkFontChooserWidget")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkFontChooserWidget")))
  ;; Check child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (gtk-test:list-child-properties "GtkFontChooserWidget")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFontChooserWidget")))
  ;; CSS information
  (is (string= "fontchooser"
               (gtk:widget-class-css-name "GtkFontChooserWidget")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFontChooserWidget"
                                      GTK:FONT-CHOOSER-WIDGET
                       (:SUPERCLASS GTK:BOX
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFontChooser"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_font_chooser_widget_get_type")
                       ((TWEAK-ACTION FONT-CHOOSER-WIDGET-TWEAK-ACTION
                         "tweak-action" "GAction" T NIL)))
             (gobject:get-gtype-definition "GtkFontChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-font-chooser-widget-properties
  (let ((widget (make-instance 'gtk:font-chooser-widget)))
    (is (typep (gtk:font-chooser-widget-tweak-action widget) 'g:simple-action))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_widget_new

(test gtk-font-chooser-widget-new
  (is (typep (gtk:font-chooser-widget-new) 'gtk:font-chooser-widget)))

;;; 2024-9-23
