(in-package :gtk-test)

(def-suite gtk-page-setup-unix-dialog :in gtk-suite)
(in-suite gtk-page-setup-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetupUnixDialog

(test gtk-page-setup-unix-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkPageSetupUnixDialog"))
  ;; Check registered name
  (is (eq 'gtk:page-setup-unix-dialog
          (glib:symbol-for-gtype "GtkPageSetupUnixDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPageSetupUnixDialog")
          (g:gtype (cffi:foreign-funcall "gtk_page_setup_unix_dialog_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog") (g:type-parent "GtkPageSetupUnixDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPageSetupUnixDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkPageSetupUnixDialog")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkPageSetupUnixDialog")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkPageSetupUnixDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkPageSetupUnixDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPageSetupUnixDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPageSetupUnixDialog"
                                      GTK:PAGE-SETUP-UNIX-DIALOG
                       (:SUPERCLASS GTK:DIALOG
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_page_setup_unix_dialog_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkPageSetupUnixDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_page_setup_unix_dialog_new

(test gtk-page-setup-unix-dialog-new.1
  (let ((window (make-instance 'gtk:window)))
    (is (eq 'gtk:page-setup-unix-dialog (type-of (gtk:page-setup-unix-dialog-new nil nil))))
    (is (eq 'gtk:page-setup-unix-dialog (type-of (gtk:page-setup-unix-dialog-new "title" nil))))
    (is (eq 'gtk:page-setup-unix-dialog (type-of (gtk:page-setup-unix-dialog-new nil window))))
    (is (eq 'gtk:page-setup-unix-dialog (type-of (gtk:page-setup-unix-dialog-new "title" window))))))

(test gtk-page-setup-unix-dialog-new.2
  (let* ((window (make-instance 'gtk:window))
         (dialog (gtk:page-setup-unix-dialog-new "title" window)))

    (is (eq 'gtk:page-setup-unix-dialog (type-of dialog)))
    (is (string= "title" (gtk:window-title dialog)))
    (is (eq window (gtk:window-transient-for dialog)))))

;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup

(test gtk-page-setup-unix-dialog-page-setup
  (let ((dialog (gtk:page-setup-unix-dialog-new "title" nil))
        (page-setup (gtk:page-setup-new)))
    (is (eq 'gtk:page-setup (type-of (gtk:page-setup-unix-dialog-page-setup dialog))))
    (is (eq page-setup (setf (gtk:page-setup-unix-dialog-page-setup dialog) page-setup)))
    ;; TODO: This should be true.
    (is-false (eq page-setup (gtk:page-setup-unix-dialog-page-setup dialog)))))

;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings

(test gtk-page-setup-unix-dialog-print-settings
  (let ((dialog (gtk:page-setup-unix-dialog-new "title" nil))
        (print-settings (gtk:print-settings-new)))
    (is (eq 'gtk:print-settings (type-of print-settings)))
    (is-false (gtk:page-setup-unix-dialog-print-settings dialog))
    (is (eq 'gtk:print-settings
            (type-of (setf (gtk:page-setup-unix-dialog-print-settings dialog) print-settings))))
    ;; TODO: This should be true.
    (is-false (eq 'gtk:print-settings
              (type-of (gtk:page-setup-unix-dialog-print-settings dialog))))))

;;; 2024-9-23
