(in-package :gtk-test)

(def-suite gtk-file-chooser-native :in gtk-suite)
(in-suite gtk-file-chooser-native)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserNative

(test gtk-file-chooser-native-class
  ;; Check type
  (is (g:type-is-object "GtkFileChooserNative"))
  ;; Check registered name
  (is (eq 'gtk:file-chooser-native
          (glib:symbol-for-gtype "GtkFileChooserNative")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooserNative")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_native_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkNativeDialog")
          (g:type-parent "GtkFileChooserNative")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFileChooserNative")))
  ;; Check interfaces
  (is (equal '("GtkFileChooser")
             (glib-test:list-interfaces "GtkFileChooserNative")))
  ;; Check the class properties
  (is (equal '("accept-label" "action" "cancel-label" "create-folders"
               "do-overwrite-confirmation" "extra-widget" "filter" "local-only"
               "preview-widget" "preview-widget-active" "select-multiple"
               "show-hidden" "use-preview-label")
             (glib-test:list-properties "GtkFileChooserNative")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFileChooserNative")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFileChooserNative"
                                      GTK:FILE-CHOOSER-NATIVE
                       (:SUPERCLASS GTK:NATIVE-DIALOG
                        :EXPORT T
                        :INTERFACES ("GtkFileChooser")
                        :TYPE-INITIALIZER "gtk_file_chooser_native_get_type")
                       ((ACCEPT-LABEL FILE-CHOOSER-NATIVE-ACCEPT-LABEL
                         "accept-label" "gchararray" T T)
                        (CANCEL-LABEL FILE-CHOOSER-NATIVE-CANCEL-LABEL
                         "cancel-label" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkFileChooserNative"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-file-chooser-native-properties
  (let ((dialog (make-instance 'gtk:file-chooser-native)))
    (is-false (gtk:file-chooser-native-accept-label dialog))
    (is-false (gtk:file-chooser-native-cancel-label dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_native_new

(test gtk-file-chooser-native-new.1
  (is (typep (gtk:file-chooser-native-new "title" nil :open "accept" "cancel")
             'gtk:file-chooser-native)))

(test gtk-file-chooser-native-new.2
  (let ((dialog (gtk:file-chooser-native-new  "title"
                                              nil
                                              :open
                                              "accept"
                                              "cancel")))
    (is (typep dialog 'gtk:file-chooser-native))
    (is (string= "title" (gtk:native-dialog-title dialog)))
    (is-false (gtk:native-dialog-transient-for dialog))
    (is (eq :open (gtk:file-chooser-action dialog)))
    (is (string= "accept" (gtk:file-chooser-native-accept-label dialog)))
    (is (string= "cancel" (gtk:file-chooser-native-cancel-label dialog)))))

;;; 2024-9-23
