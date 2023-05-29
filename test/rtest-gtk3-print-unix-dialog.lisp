(in-package :gtk-test)

(def-suite gtk-print-unix-dialog :in gtk-suite)
(in-suite gtk-print-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintCapabilities

(test print-capabilities
  ;; Check the type
  (is (g:type-is-flags "GtkPrintCapabilities"))
  ;; Check the registered name
  (is (eq 'gtk:print-capabilities
          (glib:symbol-for-gtype "GtkPrintCapabilities")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintCapabilities")
          (g:gtype (cffi:foreign-funcall "gtk_print_capabilities_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_PRINT_CAPABILITY_PAGE_SET" "GTK_PRINT_CAPABILITY_COPIES"
               "GTK_PRINT_CAPABILITY_COLLATE" "GTK_PRINT_CAPABILITY_REVERSE"
               "GTK_PRINT_CAPABILITY_SCALE" "GTK_PRINT_CAPABILITY_GENERATE_PDF"
               "GTK_PRINT_CAPABILITY_GENERATE_PS" "GTK_PRINT_CAPABILITY_PREVIEW"
               "GTK_PRINT_CAPABILITY_NUMBER_UP"
               "GTK_PRINT_CAPABILITY_NUMBER_UP_LAYOUT")
             (list-flags-item-name "GtkPrintCapabilities")))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64 128 256 512)
             (list-flags-item-value "GtkPrintCapabilities")))
  ;; Check the nick names
  (is (equal '("page-set" "copies" "collate" "reverse" "scale" "generate-pdf"
               "generate-ps" "preview" "number-up" "number-up-layout")
             (list-flags-item-nick "GtkPrintCapabilities")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkPrintCapabilities"
                              GTK-PRINT-CAPABILITIES
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
             (gobject:get-g-type-definition "GtkPrintCapabilities"))))

;;;     GtkPrintUnixDialog

(test print-unix-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkPrintUnixDialog"))
  ;; Check the registered name
  (is (eq 'gtk:print-unix-dialog
          (glib:symbol-for-gtype "GtkPrintUnixDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintUnixDialog")
          (g:gtype (cffi:foreign-funcall "gtk_print_unix_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog") (g:type-parent "GtkPrintUnixDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPrintUnixDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkPrintUnixDialog")))
  ;; Check the class properties
  (is (equal '("current-page" "embed-page-setup" "has-selection"
               "manual-capabilities" "page-setup" "print-settings"
               "selected-printer" "support-selection")
             (list-properties "GtkPrintUnixDialog")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkPrintUnixDialog")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkPrintUnixDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkPrintUnixDialog")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPrintUnixDialog" GTK-PRINT-UNIX-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_print_unix_dialog_get_type")
                       ((CURRENT-PAGE GTK-PRINT-UNIX-DIALOG-CURRENT-PAGE
                         "current-page" "gint" T T)
                        (EMBED-PAGE-SETUP
                         GTK-PRINT-UNIX-DIALOG-EMBED-PAGE-SETUP
                         "embed-page-setup" "gboolean" T T)
                        (HAS-SELECTION GTK-PRINT-UNIX-DIALOG-HAS-SELECTION
                         "has-selection" "gboolean" T T)
                        (MANUAL-CAPABILITIES
                         GTK-PRINT-UNIX-DIALOG-MANUAL-CAPABILITIES
                         "manual-capabilities" "GtkPrintCapabilities" T T)
                        (PAGE-SETUP GTK-PRINT-UNIX-DIALOG-PAGE-SETUP
                         "page-setup" "GtkPageSetup" T T)
                        (PRINT-SETTINGS GTK-PRINT-UNIX-DIALOG-PRINT-SETTINGS
                         "print-settings" "GtkPrintSettings" T T)
                        (SELECTED-PRINTER
                         GTK-PRINT-UNIX-DIALOG-SELECTED-PRINTER
                         "selected-printer" "GtkPrinter" T NIL)
                        (SUPPORT-SELECTION
                         GTK-PRINT-UNIX-DIALOG-SUPPORT-SELECTION
                         "support-selection" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkPrintUnixDialog"))))

;;; --- Properties -------------------------------------------------------------

(test print-unix-dialog-properties
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

(test print-unix-dialog-new
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

;;; --- 2023-5-29 --------------------------------------------------------------
