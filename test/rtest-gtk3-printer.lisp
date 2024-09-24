(in-package :gtk-test)

(def-suite gtk-printer :in gtk-suite)
(in-suite gtk-printer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrinter

(test gtk-printer-class
  ;; Check type
  (is (g:type-is-object "GtkPrinter"))
  ;; Check registered name
  (is (eq 'gtk:printer
          (glib:symbol-for-gtype "GtkPrinter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrinter")
          (g:gtype (cffi:foreign-funcall "gtk_printer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkPrinter")))
  ;; Check children
  (is (equal '("GtkPrinterCups")
             (glib-test:list-children "GtkPrinter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPrinter")))
  ;; Check class properties
  (is (equal '("accepting-jobs" "accepts-pdf" "accepts-ps" "backend" "icon-name"
               "is-virtual" "job-count" "location" "name" "paused"
               "state-message")
             (glib-test:list-properties "GtkPrinter")))
  ;; Check signals
  (is (equal '("details-acquired")
             (glib-test:list-signals "GtkPrinter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrinter" GTK:PRINTER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_printer_get_type")
                       ((ACCEPTING-JOBS PRINTER-ACCEPTING-JOBS
                         "accepting-jobs" "gboolean" T NIL)
                        (ACCEPTS-PDF PRINTER-ACCEPTS-PDF
                         "accepts-pdf" "gboolean" T NIL)
                        (ACCEPTS-PS PRINTER-ACCEPTS-PS
                         "accepts-ps" "gboolean" T NIL)
                        (BACKEND PRINTER-BACKEND
                         "backend" "GtkPrintBackend" T NIL)
                        (ICON-NAME PRINTER-ICON-NAME
                         "icon-name" "gchararray" T NIL)
                        (IS-VIRTUAL PRINTER-IS-VIRTUAL
                         "is-virtual" "gboolean" T NIL)
                        (JOB-COUNT PRINTER-JOB-COUNT "job-count" "gint" T NIL)
                        (LOCATION PRINTER-LOCATION "location" "gchararray" T NIL)
                        (NAME PRINTER-NAME "name" "gchararray" T NIL)
                        (PAUSED PRINTER-PAUSED "paused" "gboolean" T NIL)
                        (STATE-MESSAGE PRINTER-STATE-MESSAGE
                         "state-message" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GtkPrinter"))))

;;;     GtkPrintBackend

(test gtk-print-backend-class
  ;; Check type
  (is (g:type-is-object "GtkPrintBackend"))
  ;; Check registered name
  (is (eq 'gtk:print-backend
          (glib:symbol-for-gtype "GtkPrintBackend")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintBackend")
          (g:gtype (cffi:foreign-funcall "gtk_print_backend_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkPrintBackend")))
  ;; Check children
  (is (equal '("GtkPrintBackendCups" "GtkPrintBackendFile")
             (glib-test:list-children "GtkPrintBackend")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPrintBackend")))
  ;; Check class properties
  (is (equal '("status")
             (glib-test:list-properties "GtkPrintBackend")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrintBackend" GTK:PRINT-BACKEND
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_print_backend_get_type")
                       ((STATUS PRINT-BACKEND-STATUS "status" "gint" T T)))
             (gobject:get-gtype-definition "GtkPrintBackend"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-printer-properties
  (let* ((backend (make-instance 'gtk:print-backend))
         (printer (make-instance 'gtk:printer
                                 :name "myPrinter"
                                 :backend backend
                                 :is-virtual t)))
    ;; accepting-jobs
    (is-true (gtk:printer-accepting-jobs printer))
    (signals (error) (setf (gtk:printer-accepting-jobs printer) nil))
    ;; accepts-pdf
    (is-false (gtk:printer-accepts-pdf printer))
    ;; accepts-ps
    (is-true (gtk:printer-accepts-ps printer))
    ;; backend
    (is (eq 'gtk:print-backend (type-of (gtk:printer-backend printer))))
    ;; icon-name
    (is (string= "printer" (gtk:printer-icon-name printer)))
    (signals (error) (setf (gtk:printer-icon-name printer) nil))
    ;; is-virtual
    (is-true (gtk:printer-is-virtual printer))
    ;; job-count
    (is (= 0 (gtk:printer-job-count printer)))
    (signals (error) (setf (gtk:printer-job-count printer) 10))
    ;; location
    (is (string= "" (gtk:printer-location printer)))
    (signals (error) (setf (gtk:printer-location printer) "test"))
    ;; name
    (is (string= "myPrinter" (gtk:printer-name printer)))
    ;; paused
    (is-false (gtk:printer-paused printer))
    (signals (error) (setf (gtk:printer-paused printer) t))
    ;; state-message
    (is (string= "" (gtk:printer-state-message printer)))
    (signals (error) (setf (gtk:printer-state-message printer) "test"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_printer_new
;;;     gtk_printer_get_description
;;;     gtk_printer_is_active
;;;     gtk_printer_is_paused
;;;     gtk_printer_is_accepting_jobs
;;;     gtk_printer_is_virtual
;;;     gtk_printer_is_default
;;;     gtk_printer_accepts_ps
;;;     gtk_printer_accepts_pdf
;;;     gtk_printer_list_papers
;;;     gtk_printer_compare
;;;     gtk_printer_has_details
;;;     gtk_printer_request_details
;;;     gtk_printer_get_capabilities
;;;     gtk_printer_get_default_page_size
;;;     gtk_printer_get_hard_margins
;;;     gtk_enumerate_printers

;;; 2024-9-23
