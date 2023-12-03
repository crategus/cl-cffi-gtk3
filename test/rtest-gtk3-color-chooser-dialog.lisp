(in-package :gtk-test)

(def-suite gtk-color-chooser-dialog :in gtk-suite)
(in-suite gtk-color-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooserDialog

(test gtk-color-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkColorChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:color-chooser-dialog
          (glib:symbol-for-gtype "GtkColorChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkColorChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorChooserDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkColorChooser")
             (list-interfaces "GtkColorChooserDialog")))
  ;; Check the class properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (list-properties "GtkColorChooserDialog")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkColorChooserDialog")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkColorChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColorChooserDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkColorChooserDialog")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColorChooserDialog"
                                     GTK-COLOR-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable"
                         "GtkColorChooser")
                        :TYPE-INITIALIZER "gtk_color_chooser_dialog_get_type")
                       ((SHOW-EDITOR GTK-COLOR-CHOOSER-DIALOG-SHOW-EDITOR
                         "show-editor" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkColorChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     show-editor

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

;;; --- 2023-12-2 --------------------------------------------------------------

