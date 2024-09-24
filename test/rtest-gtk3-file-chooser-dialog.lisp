(in-package :gtk-test)

(def-suite gtk-file-chooser-dialog :in gtk-suite)
(in-suite gtk-file-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserDialog

(test gtk-file-chooser-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkFileChooserDialog"))
  ;; Check registered name
  (is (eq 'gtk:file-chooser-dialog
          (glib:symbol-for-gtype "GtkFileChooserDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkFileChooserDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFileChooserDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
             (glib-test:list-interfaces "GtkFileChooserDialog")))
  ;; Check class properties
  (is (equal '("action" "create-folders" "do-overwrite-confirmation"
               "extra-widget" "filter" "local-only" "preview-widget"
               "preview-widget-active" "select-multiple" "show-hidden"
               "use-preview-label")
             (glib-test:list-properties "GtkFileChooserDialog")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkFileChooserDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkFileChooserDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFileChooserDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkFileChooserDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFileChooserDialog"
                                      GTK:FILE-CHOOSER-DIALOG
                       (:SUPERCLASS GTK:DIALOG
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
                        :TYPE-INITIALIZER "gtk_file_chooser_dialog_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkFileChooserDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_dialog_new

(test gtk-file-chooser-dialog-new
  (let ((dialog nil))
    (is (typep (setf dialog
                     (gtk:file-chooser-dialog-new "title"
                                                  nil
                                                  :save
                                                  "_OK"
                                                  :accept
                                                  "_Canel"
                                                  :reject))
               'gtk:file-chooser-dialog))))

;;; 2024-9-23
