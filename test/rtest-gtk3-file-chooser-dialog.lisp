(in-package :gtk-test)

(def-suite gtk-file-chooser-dialog :in gtk-suite)
(in-suite gtk-file-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserDialog

(test gtk-file-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkFileChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser-dialog
          (glib:symbol-for-gtype "GtkFileChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkFileChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileChooserDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
             (list-interfaces "GtkFileChooserDialog")))
  ;; Check the class properties
  (is (equal '("action" "create-folders" "do-overwrite-confirmation"
               "extra-widget" "filter" "local-only" "preview-widget"
               "preview-widget-active" "select-multiple" "show-hidden"
               "use-preview-label")
             (list-properties "GtkFileChooserDialog")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkFileChooserDialog")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkFileChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFileChooserDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkFileChooserDialog")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileChooserDialog"
                                     GTK-FILE-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
                        :TYPE-INITIALIZER "gtk_file_chooser_dialog_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFileChooserDialog"))))

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

;;; --- 2023-6-11 --------------------------------------------------------------
