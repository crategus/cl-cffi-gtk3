(in-package :gtk-test)

(def-suite gtk-print-unix-dialog :in gtk-suite)
(in-suite gtk-print-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintCapabilities

(test gtk-print-capabilities
  ;; Check type
  (is (g:type-is-flags "GtkPrintCapabilities"))
  ;; Check registered name
  (is (eq 'gtk:print-capabilities
          (glib:symbol-for-gtype "GtkPrintCapabilities")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintCapabilities")
          (g:gtype (cffi:foreign-funcall "gtk_print_capabilities_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_PRINT_CAPABILITY_PAGE_SET" "GTK_PRINT_CAPABILITY_COPIES"
               "GTK_PRINT_CAPABILITY_COLLATE" "GTK_PRINT_CAPABILITY_REVERSE"
               "GTK_PRINT_CAPABILITY_SCALE" "GTK_PRINT_CAPABILITY_GENERATE_PDF"
               "GTK_PRINT_CAPABILITY_GENERATE_PS" "GTK_PRINT_CAPABILITY_PREVIEW"
               "GTK_PRINT_CAPABILITY_NUMBER_UP"
               "GTK_PRINT_CAPABILITY_NUMBER_UP_LAYOUT")
             (glib-test:list-flags-item-names "GtkPrintCapabilities")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 64 128 256 512)
             (glib-test:list-flags-item-values "GtkPrintCapabilities")))
  ;; Check nick names
  (is (equal '("page-set" "copies" "collate" "reverse" "scale" "generate-pdf"
               "generate-ps" "preview" "number-up" "number-up-layout")
             (glib-test:list-flags-item-nicks "GtkPrintCapabilities")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkPrintCapabilities"
                                     GTK:PRINT-CAPABILITIES
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_print_capabilities_get_type")
                       (:PAGE-SET 1)
                       (:COPIES 2)
                       (:COLLATE 4)
                       (:REVERSE 8)
                       (:SCALE 16)
                       (:GENERATE-PDF 32)
                       (:GENERATE-PS 64)
                       (:PREVIEW 128)
                       (:NUMBER-UP 256)
                       (:NUMBER-UP-LAYOUT 512))
             (gobject:get-gtype-definition "GtkPrintCapabilities"))))

;;;     GtkPrintUnixDialog

(test gtk-print-unix-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkPrintUnixDialog"))
  ;; Check registered name
  (is (eq 'gtk:print-unix-dialog
          (glib:symbol-for-gtype "GtkPrintUnixDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintUnixDialog")
          (g:gtype (cffi:foreign-funcall "gtk_print_unix_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog") (g:type-parent "GtkPrintUnixDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPrintUnixDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkPrintUnixDialog")))
  ;; Check class properties
  (is (equal '("current-page" "embed-page-setup" "has-selection"
               "manual-capabilities" "page-setup" "print-settings"
               "selected-printer" "support-selection")
             (glib-test:list-properties "GtkPrintUnixDialog")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkPrintUnixDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkPrintUnixDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPrintUnixDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrintUnixDialog" GTK:PRINT-UNIX-DIALOG
                       (:SUPERCLASS GTK:DIALOG
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_print_unix_dialog_get_type")
                       ((CURRENT-PAGE PRINT-UNIX-DIALOG-CURRENT-PAGE
                         "current-page" "gint" T T)
                        (EMBED-PAGE-SETUP PRINT-UNIX-DIALOG-EMBED-PAGE-SETUP
                         "embed-page-setup" "gboolean" T T)
                        (HAS-SELECTION PRINT-UNIX-DIALOG-HAS-SELECTION
                         "has-selection" "gboolean" T T)
                        (MANUAL-CAPABILITIES
                         PRINT-UNIX-DIALOG-MANUAL-CAPABILITIES
                         "manual-capabilities" "GtkPrintCapabilities" T T)
                        (PAGE-SETUP PRINT-UNIX-DIALOG-PAGE-SETUP
                         "page-setup" "GtkPageSetup" T T)
                        (PRINT-SETTINGS PRINT-UNIX-DIALOG-PRINT-SETTINGS
                         "print-settings" "GtkPrintSettings" T T)
                        (SELECTED-PRINTER PRINT-UNIX-DIALOG-SELECTED-PRINTER
                         "selected-printer" "GtkPrinter" T NIL)
                        (SUPPORT-SELECTION PRINT-UNIX-DIALOG-SUPPORT-SELECTION
                         "support-selection" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkPrintUnixDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-print-unix-dialog-properties
  (let ((dialog (make-instance 'gtk:print-unix-dialog)))
    ;; current-page
    (is (= -1 (gtk:print-unix-dialog-current-page dialog)))
    (is (= 10 (setf (gtk:print-unix-dialog-current-page dialog) 10)))
    (is (= 10 (gtk:print-unix-dialog-current-page dialog)))
    ;; embed-page-setup
    (is-false (gtk:print-unix-dialog-embed-page-setup dialog))
    (is-true (setf (gtk:print-unix-dialog-embed-page-setup dialog) t))
    (is-true (gtk:print-unix-dialog-embed-page-setup dialog))
    ;; has-selection
    (is-false (gtk:print-unix-dialog-has-selection dialog))
    (is-true (setf (gtk:print-unix-dialog-has-selection dialog) t))
    (is-true (gtk:print-unix-dialog-has-selection dialog))
    ;; manual-capabilities
    (is-false (gtk:print-unix-dialog-manual-capabilities dialog))
    (is (equal '(:page-set :scale)
               (setf (gtk:print-unix-dialog-manual-capabilities dialog)
                     '(:page-set :scale))))
    (is (equal '(:page-set :scale)
               (gtk:print-unix-dialog-manual-capabilities dialog)))
    ;; page-setup
    (is (eq 'gtk:page-setup (type-of (gtk:print-unix-dialog-page-setup dialog))))
    (is (eq 'gtk:page-setup
            (type-of (setf (gtk:print-unix-dialog-page-setup dialog)
                           (make-instance 'gtk:page-setup)))))
    (is (eq 'gtk:page-setup (type-of (gtk:print-unix-dialog-page-setup dialog))))
    ;; print-settings
    (is (typep (gtk:print-unix-dialog-print-settings dialog) 'gtk:print-settings))
    (is (typep (setf (gtk:print-unix-dialog-print-settings dialog)
                     (make-instance 'gtk:print-settings))
               'gtk:print-settings))
    (is (typep (gtk:print-unix-dialog-print-settings dialog) 'gtk:print-settings))
    ;; selected-printer
    (is-false (gtk:print-unix-dialog-selected-printer dialog))
    ;; selected-printer is not writeable
    (signals (error)
             (setf (gtk:print-unix-dialog-selected-printer dialog)
                   (make-instance 'gtk:printer)))
    ;; support-selection
    (is-false (gtk:print-unix-dialog-support-selection dialog))
    (is-true (setf (gtk:print-unix-dialog-support-selection dialog) t))
    (is-true (gtk:print-unix-dialog-support-selection dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_unix_dialog_new

(test gtk-print-unix-dialog-new
  (let ((window (make-instance 'gtk:window)))
    (is (typep (gtk:print-unix-dialog-new nil nil) 'gtk:print-unix-dialog))
    (is (typep (gtk:print-unix-dialog-new "title" window) 'gtk:print-unix-dialog))
    (is (typep (gtk:print-unix-dialog-new nil window) 'gtk:print-unix-dialog))
    (is (typep (gtk:print-unix-dialog-new "title" window)
               'gtk:print-unix-dialog))))

;;;     gtk_print_unix_dialog_set_settings
;;;     gtk_print_unix_dialog_get_settings
;;;     gtk_print_unix_dialog_add_custom_tab
;;;     gtk_print_unix_dialog_get_page_setup_set

;;; 2024-9-21
