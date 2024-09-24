(in-package :gtk-test)

(def-suite gtk-app-chooser-dialog :in gtk-suite)
(in-suite gtk-app-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserDialog

(test gtk-app-chooser-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkAppChooserDialog"))
  ;; Check registered name
  (is (eq 'gtk:app-chooser-dialog
          (glib:symbol-for-gtype "GtkAppChooserDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAppChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog") (g:type-parent "GtkAppChooserDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAppChooserDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkAppChooser")
             (glib-test:list-interfaces "GtkAppChooserDialog")))
  ;; Check class properties
  (is (equal '("content-type" "gfile" "heading")
             (glib-test:list-properties "GtkAppChooserDialog")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkAppChooserDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkAppChooserDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAppChooserDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAppChooserDialog"
                                      GTK:APP-CHOOSER-DIALOG
                       (:SUPERCLASS GTK:DIALOG
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkAppChooser" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_app_chooser_dialog_get_type")
                       ((GFILE APP-CHOOSER-DIALOG-GFILE "gfile" "GFile" T NIL)
                        (HEADING APP-CHOOSER-DIALOG-HEADING
                         "heading" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkAppChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-dialog-properties
 (let ((chooser (make-instance 'gtk:app-chooser-dialog
                               :heading "<b>header</b>")))
    (is-false (gtk:app-chooser-dialog-gfile chooser))
    (is (string= "<b>header</b>" (gtk:app-chooser-dialog-heading chooser)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_dialog_new

(test gtk-app-chooser-dialog-new
  (let* ((path (glib-sys:sys-path "test/rtest-gtk3-app-chooser-dialog.lisp"))
         (filename (namestring path))
         (chooser (gtk:app-chooser-dialog-new nil
                                              '(:modal)
                                              (g:file-new-for-path filename))))
    (is (typep chooser 'gtk:app-chooser-dialog))
    (is (typep (gtk:app-chooser-dialog-gfile chooser) 'g:object))
    (is (string= "rtest-gtk3-app-chooser-dialog.lisp"
                 (g:file-basename (gtk:app-chooser-dialog-gfile chooser))))))

;;;     gtk_app_chooser_dialog_new_for_content_type

(test gtk-app-chooser-dialog-new-for-content-type
  (let ((chooser (gtk:app-chooser-dialog-new-for-content-type nil
                                                              '(:modal)
                                                              "plain/text")))
    (is (typep chooser 'gtk:app-chooser-dialog))))

;;;     gtk_app_chooser_dialog_get_widget

(test gtk-app-chooser-dialog-widget
  (let ((chooser (make-instance 'gtk:app-chooser-dialog)))
    (is (typep (gtk:app-chooser-dialog-widget chooser) 'gtk:widget))))

;;; 2024-9-21
