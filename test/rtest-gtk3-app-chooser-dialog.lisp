(in-package :gtk-test)

(def-suite gtk-app-chooser-dialog :in gtk-suite)
(in-suite gtk-app-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserDialog

(test app-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkAppChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:app-chooser-dialog
          (gobject:symbol-for-gtype "GtkAppChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAppChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog") (g:type-parent "GtkAppChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAppChooserDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkAppChooser")
             (list-interfaces "GtkAppChooserDialog")))
  ;; Check the class properties
  (is (equal '("content-type" "gfile" "heading")
             (list-properties "GtkAppChooserDialog")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkAppChooserDialog")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkAppChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAppChooserDialog")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAppChooserDialog" GTK-APP-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkAppChooser" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_app_chooser_dialog_get_type")
                       ((GFILE GTK-APP-CHOOSER-DIALOG-GFILE "gfile" "GFile" T
                         NIL)
                        (HEADING GTK-APP-CHOOSER-DIALOG-HEADING "heading"
                         "gchararray" T T)))
             (gobject:get-g-type-definition "GtkAppChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

(test app-chooser-dialog-properties
 (let ((chooser (make-instance 'gtk:app-chooser-dialog
                               :heading "<b>header</b>")))
    (is-false (gtk:app-chooser-dialog-gfile chooser))
    (is (string= "<b>header</b>" (gtk:app-chooser-dialog-heading chooser)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_dialog_new

(test app-chooser-dialog-new
  (let* ((filename (namestring (sys-path "rtest-gtk3-app-chooser-dialog.lisp")))
         (chooser (gtk:app-chooser-dialog-new nil
                                              '(:modal)
                                              (g:file-new-for-path filename))))
    (is (typep chooser 'gtk:app-chooser-dialog))
    (is (typep (gtk:app-chooser-dialog-gfile chooser) 'g:object))
    (is (string= "rtest-gtk3-app-chooser-dialog.lisp"
                 (g:file-basename (gtk:app-chooser-dialog-gfile chooser))))))

;;;     gtk_app_chooser_dialog_new_for_content_type

(test app-chooser-dialog-new-for-content-type
  (let ((chooser (gtk:app-chooser-dialog-new-for-content-type nil
                                                              '(:modal)
                                                              "plain/text")))
    (is (typep chooser 'gtk:app-chooser-dialog))))

;;;     gtk_app_chooser_dialog_get_widget

(test app-chooser-dialog-widget
  (let ((chooser (make-instance 'gtk:app-chooser-dialog)))
    (is (typep (gtk:app-chooser-dialog-widget chooser) 'gtk:widget))))

;;; --- 2023-2-18 --------------------------------------------------------------
