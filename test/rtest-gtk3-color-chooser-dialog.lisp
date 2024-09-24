(in-package :gtk-test)

(def-suite gtk-color-chooser-dialog :in gtk-suite)
(in-suite gtk-color-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooserDialog

(test gtk-color-chooser-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkColorChooserDialog"))
  ;; Check registered name
  (is (eq 'gtk:color-chooser-dialog
          (glib:symbol-for-gtype "GtkColorChooserDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColorChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_dialog_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkColorChooserDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkColorChooserDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkColorChooser")
             (glib-test:list-interfaces "GtkColorChooserDialog")))
  ;; Check class properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (glib-test:list-properties "GtkColorChooserDialog")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkColorChooserDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkColorChooserDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkColorChooserDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkColorChooserDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkColorChooserDialog"
                                       GTK:COLOR-CHOOSER-DIALOG
                       (:SUPERCLASS GTK:DIALOG
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable"
                         "GtkColorChooser")
                        :TYPE-INITIALIZER "gtk_color_chooser_dialog_get_type")
                       ((SHOW-EDITOR COLOR-CHOOSER-DIALOG-SHOW-EDITOR
                         "show-editor" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkColorChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-color-chooser-dialog-properties
  (let ((dialog (make-instance 'gtk:color-chooser-dialog)))
    (is-false (gtk:color-chooser-dialog-show-editor dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_dialog_new

(test gtk-color-chooser-dialog-new.1
  (is (typep (gtk:color-chooser-dialog-new "title" nil)
             'gtk:color-chooser-dialog)))

;; TODO: We get a warning:
;;   (sbcl:13528): Gtk-WARNING **: Can't set a parent on a toplevel widget

#+nil
(test gtk-color-chooser-dialog-new.2
  (is (typep (gtk:color-chooser-dialog-new "title" (gtk:window-new :toplevel))
             'gtk:color-chooser-dialog)))

;;; 2024-9-23

