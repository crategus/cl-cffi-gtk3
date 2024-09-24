(in-package :gtk-test)

(def-suite gtk-print-job :in gtk-suite)
(in-suite gtk-print-job)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintJob

(test gtk-print-job-class
  ;; Check type
  (is (g:type-is-object "GtkPrintJob"))
  ;; Check registered name
  (is (eq 'gtk:print-job
          (glib:symbol-for-gtype "GtkPrintJob")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintJob")
          (g:gtype (cffi:foreign-funcall "gtk_print_job_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkPrintJob")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPrintJob")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPrintJob")))
  ;; Check class properties
  (is (equal '("page-setup" "printer" "settings" "title" "track-print-status")
             (glib-test:list-properties "GtkPrintJob")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrintJob" GTK:PRINT-JOB
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_print_job_get_type")
                       ((PAGE-SETUP PRINT-JOB-PAGE-SETUP
                         "page-setup" "GtkPageSetup" T NIL)
                        (PRINTER PRINT-JOB-PRINTER "printer" "GtkPrinter" T NIL)
                        (SETTINGS PRINT-JOB-SETTINGS
                         "settings" "GtkPrintSettings" T NIL)
                        (TITLE PRINT-JOB-TITLE "title" "gchararray" T NIL)
                        (TRACK-PRINT-STATUS PRINT-JOB-TRACK-PRINT-STATUS
                         "track-print-status" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkPrintJob"))))

;;; --- Properties -------------------------------------------------------------

;; TODO: We cannot create an instance of GTKPrintJob.

;(test gtk-print-job-properties
;  (let ((job (make-instance 'gtk-print-job
;                            :page-setup (gtk-page-setup-new)
;                            :printer (gtk-printer-new "Printer" nil nil)
;                            :settings (gtk-print-settings-new)
;                            :title "Print Job"
;                            :track-print-status t)))
    ;; page-setup
;    (is-false (gtk-print-job-page-setup job))
    ;; printer
;    (is-false (gtk-print-job-printer job))
    ;; settings
;    (is-false (gtk-print-job-settings job))
    ;; title
;    (is-false (gtk-print-job-title job))
    ;; track-print-status
;    (is-false (gtk-print-job-track-print-status job))
;))


;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_job_new

;; TODO: Does not work. Find a working example.

#+nil
(test gtk-print-job-new
  (let ((printer (gtk:printer-new "printer" nil nil))
        (settings (gtk:print-settings-new))
        (setup (gtk:page-setup-new)))

    (is-false (gtk:print-job-new "job" printer settings setup))

))

;;;     gtk_print_job_get_settings
;;;     gtk_print_job_get_printer
;;;     gtk_print_job_get_title
;;;     gtk_print_job_get_status
;;;     gtk_print_job_set_source_fd
;;;     gtk_print_job_set_source_file
;;;     gtk_print_job_get_surface
;;;     gtk_print_job_send
;;;     gtk_print_job_set_track_print_status
;;;     gtk_print_job_get_track_print_status
;;;     gtk_print_job_get_pages
;;;     gtk_print_job_set_pages
;;;     gtk_print_job_get_page_ranges
;;;     gtk_print_job_set_page_ranges
;;;     gtk_print_job_get_page_set
;;;     gtk_print_job_set_page_set
;;;     gtk_print_job_get_num_copies
;;;     gtk_print_job_set_num_copies
;;;     gtk_print_job_get_scale
;;;     gtk_print_job_set_scale
;;;     gtk_print_job_get_n_up
;;;     gtk_print_job_set_n_up
;;;     gtk_print_job_get_n_up_layout
;;;     gtk_print_job_set_n_up_layout
;;;     gtk_print_job_get_rotate
;;;     gtk_print_job_set_rotate
;;;     gtk_print_job_get_collate
;;;     gtk_print_job_set_collate
;;;     gtk_print_job_get_reverse
;;;     gtk_print_job_set_reverse

;;; 2024-9-21
