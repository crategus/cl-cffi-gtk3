(in-package :gtk-test)

(def-suite gtk-page-setup-unix-dialog :in gtk-suite)
(in-suite gtk-page-setup-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetupUnixDialog

(test page-setup-unix-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkPageSetupUnixDialog"))
  ;; Check the registered name
  (is (eq 'gtk:page-setup-unix-dialog
          (gobject:symbol-for-gtype "GtkPageSetupUnixDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPageSetupUnixDialog")
          (g:gtype (cffi:foreign-funcall "gtk_page_setup_unix_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog") (g:type-parent "GtkPageSetupUnixDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPageSetupUnixDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkPageSetupUnixDialog")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkPageSetupUnixDialog")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkPageSetupUnixDialog")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkPageSetupUnixDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkPageSetupUnixDialog")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPageSetupUnixDialog"
                                     GTK-PAGE-SETUP-UNIX-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER
                        "gtk_page_setup_unix_dialog_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkPageSetupUnixDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_page_setup_unix_dialog_new

(test page-setup-unix-dialog-new.1
  (let ((window (make-instance 'gtk:window)))
    (is (eq 'gtk:page-setup-unix-dialog (type-of (gtk:page-setup-unix-dialog-new nil nil))))
    (is (eq 'gtk:page-setup-unix-dialog (type-of (gtk:page-setup-unix-dialog-new "title" nil))))
    (is (eq 'gtk:page-setup-unix-dialog (type-of (gtk:page-setup-unix-dialog-new nil window))))
    (is (eq 'gtk:page-setup-unix-dialog (type-of (gtk:page-setup-unix-dialog-new "title" window))))))

(test page-setup-unix-dialog-new.2
  (let* ((window (make-instance 'gtk:window))
         (dialog (gtk:page-setup-unix-dialog-new "title" window)))

    (is (eq 'gtk:page-setup-unix-dialog (type-of dialog)))
    (is (string= "title" (gtk:window-title dialog)))
    (is (eq window (gtk:window-transient-for dialog)))))

;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup

(test page-setup-unix-dialog-page-setup
  (let ((dialog (gtk:page-setup-unix-dialog-new "title" nil))
        (page-setup (gtk:page-setup-new)))
    (is (eq 'gtk:page-setup (type-of (gtk:page-setup-unix-dialog-page-setup dialog))))
    (is (eq page-setup (setf (gtk:page-setup-unix-dialog-page-setup dialog) page-setup)))
    ;; TODO: This should be true.
    (is-false (eq page-setup (gtk:page-setup-unix-dialog-page-setup dialog)))))

;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings

(test page-setup-unix-dialog-print-settings
  (let ((dialog (gtk:page-setup-unix-dialog-new "title" nil))
        (print-settings (gtk:print-settings-new)))
    (is (eq 'gtk:print-settings (type-of print-settings)))
    (is-false (gtk:page-setup-unix-dialog-print-settings dialog))
    (is (eq 'gtk:print-settings
            (type-of (setf (gtk:page-setup-unix-dialog-print-settings dialog) print-settings))))
    ;; TODO: This should be true.
    (is-false (eq 'gtk:print-settings
              (type-of (gtk:page-setup-unix-dialog-print-settings dialog))))))

;;; --- 2023-1-1 ---------------------------------------------------------------
