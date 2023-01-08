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
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_dialog_get_type" :size))))
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

;;;     GFile*   gfile      Read / Write / Construct Only
;;;     gchar*   heading    Read / Write

(test app-chooser-dialog-properties
 (let ((chooser (make-instance 'gtk:app-chooser-dialog)))
    (is-false (gtk:app-chooser-dialog-gfile chooser))
    (is-false (gtk:app-chooser-dialog-heading chooser))
))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_dialog_new

;; FIXME: What is the problem?
;; GLib-GIO-CRITICAL: g_file_info_get_content_type: assertion 'G_IS_FILE_INFO (info)' failed

#+nil
(test app-chooser-dialog-new
  (let ((chooser (gtk:app-chooser-dialog-new nil '(:modal) (g:file-new-for-path "gio.file.lisp"))))

    (is (eq 'gtk:app-chooser-dialog (type-of chooser)))

    (is (eq 'g-object (type-of (gtk:app-chooser-dialog-gfile chooser))))
    (is (string= "/home/dieter/Lisp/lisp-projects/cl-gtk/test/gio.file.lisp"
                 (g:file-path (gtk:app-chooser-dialog-gfile chooser))))))

;;;     gtk_app_chooser_dialog_new_for_content_type

(test app-chooser-dialog-new-for-content-type
  (let ((chooser (gtk:app-chooser-dialog-new-for-content-type nil '(:modal) "plain/text")))
    (is (eq 'gtk:app-chooser-dialog (type-of chooser)))))

;;;     gtk_app_chooser_dialog_get_widget

(test app-chooser-dialog-widget
  (is (eq 'gtk:app-chooser-widget
          (type-of (gtk:app-chooser-dialog-widget (make-instance 'gtk:app-chooser-dialog))))))

;;; --- 2023-1-1 ---------------------------------------------------------------
