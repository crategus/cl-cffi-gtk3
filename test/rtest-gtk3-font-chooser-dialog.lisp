(in-package :gtk-test)

(def-suite gtk-font-chooser-dialog :in gtk-suite)
(in-suite gtk-font-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserDialog

(test gtk-font-chooser-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkFontChooserDialog"))
  ;; Check registered name
  (is (eq 'gtk:font-chooser-dialog
          (glib:symbol-for-gtype "GtkFontChooserDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkFontChooserDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFontChooserDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkFontChooser")
             (glib-test:list-interfaces "GtkFontChooserDialog")))
  ;; Check class properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry")
             (glib-test:list-properties "GtkFontChooserDialog")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkFontChooserDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkFontChooserDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFontChooserDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkFontChooserDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFontChooserDialog"
                                       GTK:FONT-CHOOSER-DIALOG
                       (:SUPERCLASS GTK:DIALOG
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFontChooser")
                        :TYPE-INITIALIZER "gtk_font_chooser_dialog_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkFontChooserDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_dialog_new

(test gtk-font-chooser-dialog-new.1
  (is (typep (gtk:font-chooser-dialog-new "title" nil)
             'gtk:font-chooser-dialog)))

(test gtk-font-chooser-dialog-new.2
  (is (typep (gtk:font-chooser-dialog-new nil nil)
             'gtk:font-chooser-dialog)))

;;; 2024-9-23
