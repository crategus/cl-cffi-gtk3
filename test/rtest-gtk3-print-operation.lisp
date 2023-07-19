(in-package :gtk-test)

(def-suite gtk-print-operation :in gtk-suite)
(in-suite gtk-print-operation)

;;;     GtkPrintStatus

(test print-status
  ;; Check the type
  (is-true (g:type-is-enum "GtkPrintStatus"))
  ;; Check the registered name
  (is (eq 'gtk:print-status
          (glib:symbol-for-gtype "GtkPrintStatus")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintStatus")
          (g:gtype (cffi:foreign-funcall "gtk_print_status_get_type" :size))))
  ;; Check the names
  (is (equal '("GTK_PRINT_STATUS_INITIAL" "GTK_PRINT_STATUS_PREPARING"
               "GTK_PRINT_STATUS_GENERATING_DATA"
               "GTK_PRINT_STATUS_SENDING_DATA" "GTK_PRINT_STATUS_PENDING"
               "GTK_PRINT_STATUS_PENDING_ISSUE" "GTK_PRINT_STATUS_PRINTING"
               "GTK_PRINT_STATUS_FINISHED" "GTK_PRINT_STATUS_FINISHED_ABORTED")
             (list-enum-item-name "GtkPrintStatus")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (list-enum-item-value "GtkPrintStatus")))
  ;; Check the nick names
  (is (equal '("initial" "preparing" "generating-data" "sending-data" "pending"
               "pending-issue" "printing" "finished" "finished-aborted")
             (list-enum-item-nick "GtkPrintStatus")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPrintStatus"
                             GTK-PRINT-STATUS
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_print_status_get_type")
                             (:INITIAL 0)
                             (:PREPARING 1)
                             (:GENERATING-DATA 2)
                             (:SENDING-DATA 3)
                             (:PENDING 4)
                             (:PENDING-ISSUE 5)
                             (:PRINTING 6)
                             (:FINISHED 7)
                             (:FINISHED-ABORTED 8))
             (gobject:get-g-type-definition "GtkPrintStatus"))))

;;;     GtkPrintOperationAction

(test print-operation-action
  ;; Check the type
  (is-true (g:type-is-enum "GtkPrintOperationAction"))
  ;; Check the registered name
  (is (eq 'gtk:print-operation-action
          (glib:symbol-for-gtype "GtkPrintOperationAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperationAction")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_action_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG"
               "GTK_PRINT_OPERATION_ACTION_PRINT"
               "GTK_PRINT_OPERATION_ACTION_PREVIEW"
               "GTK_PRINT_OPERATION_ACTION_EXPORT")
             (list-enum-item-name "GtkPrintOperationAction")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkPrintOperationAction")))
  ;; Check the nick names
  (is (equal '("print-dialog" "print" "preview" "export")
             (list-enum-item-nick "GtkPrintOperationAction")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPrintOperationAction"
                             GTK-PRINT-OPERATION-ACTION
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_print_operation_action_get_type")
                             (:PRINT-DIALOG 0)
                             (:PRINT 1)
                             (:PREVIEW 2)
                             (:EXPORT 3))
             (gobject:get-g-type-definition "GtkPrintOperationAction"))))

;;;     GtkPrintOperationResult

(test print-operation-result
  ;; Check the type
  (is-true (g:type-is-enum "GtkPrintOperationResult"))
  ;; Check the registered name
  (is (eq 'gtk:print-operation-result
          (glib:symbol-for-gtype "GtkPrintOperationResult")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperationResult")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_result_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_PRINT_OPERATION_RESULT_ERROR"
               "GTK_PRINT_OPERATION_RESULT_APPLY"
               "GTK_PRINT_OPERATION_RESULT_CANCEL"
               "GTK_PRINT_OPERATION_RESULT_IN_PROGRESS")
             (list-enum-item-name "GtkPrintOperationResult")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkPrintOperationResult")))
  ;; Check the nick names
  (is (equal '("error" "apply" "cancel" "in-progress")
             (list-enum-item-nick "GtkPrintOperationResult")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPrintOperationResult"
                             GTK-PRINT-OPERATION-RESULT
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_print_operation_result_get_type")
                             (:ERROR 0)
                             (:APPLY 1)
                             (:CANCEL 2)
                             (:IN-PROGRESS 3))
             (gobject:get-g-type-definition "GtkPrintOperationResult"))))

;;;     GtkPrintError                                      not exported

#+nil
(test print-error
  ;; Check the type
  (is-true (g:type-is-enum "GtkPrintError"))
  ;; Check the registered name
  (is (eq 'gtk:print-error
          (glib:symbol-for-gtype "GtkPrintError")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintError")
          (g:gtype (cffi:foreign-funcall "gtk_print_error_get_type" :size))))
  ;; Check the names
  (is (equal '("GTK_PRINT_ERROR_GENERAL" "GTK_PRINT_ERROR_INTERNAL_ERROR"
               "GTK_PRINT_ERROR_NOMEM" "GTK_PRINT_ERROR_INVALID_FILE")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GtkPrintError"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GtkPrintError"))))
  ;; Check the nick names
  (is (equal '("general" "internal-error" "nomem" "invalid-file")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GtkPrintError"))))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPrintError"
                             GTK-PRINT-ERROR
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_print_error_get_type")
                             (:GENERAL 0)
                             (:INTERNAL-ERROR 1)
                             (:NOMEM 2)
                             (:INVALID-FILE 3))
             (gobject::get-g-type-definition "GtkPrintError"))))

;;;     GtkPrintOperationPreview

(test gtk-print-operation-preview-interface
  ;; Type check
  (is-true (g:type-is-interface "GtkPrintOperationPreview"))
  ;; Check the registered name
  (is (eq 'gtk:print-operation-preview
          (glib:symbol-for-gtype "GtkPrintOperationPreview")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperationPreview")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_preview_get_type"
                                         :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkPrintOperationPreview")))
  ;; Check the signals
  (is (equal '("got-page-size" "ready")
             (list-signals "GtkPrintOperationPreview")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkPrintOperationPreview"
                            GTK-PRINT-OPERATION-PREVIEW
                            (:EXPORT T :TYPE-INITIALIZER
                             "gtk_print_operation_preview_get_type"))
             (gobject:get-g-type-definition "GtkPrintOperationPreview"))))

;;;     Signals

(test gtk-print-operation-preview-got-page-size-signal
  (let ((query (g:signal-query (g:signal-lookup "got-page-size"
                                                "GtkPrintOperationPreview"))))
    (is (string= "got-page-size" (g:signal-query-signal-name query)))
    (is (string= "GtkPrintOperationPreview"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkPrintContext" "GtkPageSetup")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

(test gtk-print-operation-preview-ready-signal
  (let ((query (g:signal-query (g:signal-lookup "ready"
                                                "GtkPrintOperationPreview"))))
    (is (string= "ready" (g:signal-query-signal-name query)))
    (is (string= "GtkPrintOperationPreview"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkPrintContext")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     GtkPrintOperation

(test print-operation-class
  ;; Type check
  (is (g:type-is-object "GtkPrintOperation"))
  ;; Check the registered name
  (is (eq 'gtk:print-operation
          (glib:symbol-for-gtype "GtkPrintOperation")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperation")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintOperation")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPrintOperation")))
  ;; Check the interfaces
  (is (equal '("GtkPrintOperationPreview")
             (list-interfaces "GtkPrintOperation")))
  ;; Check the class properties
  (is (equal '("allow-async" "current-page" "custom-tab-label"
               "default-page-setup" "embed-page-setup" "export-filename"
               "has-selection" "job-name" "n-pages" "n-pages-to-print"
               "print-settings" "show-progress" "status" "status-string"
               "support-selection" "track-print-status" "unit" "use-full-page")
             (list-properties "GtkPrintOperation")))
  ;; Check the signals
  (is (equal '("begin-print" "create-custom-widget" "custom-widget-apply" "done"
               "draw-page" "end-print" "paginate" "preview" "request-page-setup"
               "status-changed" "update-custom-widget")
             (list-signals "GtkPrintOperation")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPrintOperation" GTK-PRINT-OPERATION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkPrintOperationPreview") :TYPE-INITIALIZER
                        "gtk_print_operation_get_type")
                       ((ALLOW-ASYNC GTK-PRINT-OPERATION-ALLOW-ASYNC
                         "allow-async" "gboolean" T T)
                        (CURRENT-PAGE GTK-PRINT-OPERATION-CURRENT-PAGE
                         "current-page" "gint" T T)
                        (CUSTOM-TAB-LABEL GTK-PRINT-OPERATION-CUSTOM-TAB-LABEL
                         "custom-tab-label" "gchararray" T T)
                        (DEFAULT-PAGE-SETUP
                         GTK-PRINT-OPERATION-DEFAULT-PAGE-SETUP
                         "default-page-setup" "GtkPageSetup" T T)
                        (EMBED-PAGE-SETUP GTK-PRINT-OPERATION-EMBED-PAGE-SETUP
                         "embed-page-setup" "gboolean" T T)
                        (EXPORT-FILENAME GTK-PRINT-OPERATION-EXPORT-FILENAME
                         "export-filename" "gchararray" T T)
                        (HAS-SELECTION GTK-PRINT-OPERATION-HAS-SELECTION
                         "has-selection" "gboolean" T T)
                        (JOB-NAME GTK-PRINT-OPERATION-JOB-NAME "job-name"
                         "gchararray" T T)
                        (N-PAGES GTK-PRINT-OPERATION-N-PAGES "n-pages" "gint" T
                         T)
                        (N-PAGES-TO-PRINT GTK-PRINT-OPERATION-N-PAGES-TO-PRINT
                         "n-pages-to-print" "gint" T NIL)
                        (PRINT-SETTINGS GTK-PRINT-OPERATION-PRINT-SETTINGS
                         "print-settings" "GtkPrintSettings" T T)
                        (SHOW-PROGRESS GTK-PRINT-OPERATION-SHOW-PROGRESS
                         "show-progress" "gboolean" T T)
                        (STATUS GTK-PRINT-OPERATION-STATUS "status"
                         "GtkPrintStatus" T NIL)
                        (STATUS-STRING GTK-PRINT-OPERATION-STATUS-STRING
                         "status-string" "gchararray" T NIL)
                        (SUPPORT-SELECTION
                         GTK-PRINT-OPERATION-SUPPORT-SELECTION
                         "support-selection" "gboolean" T T)
                        (TRACK-PRINT-STATUS
                         GTK-PRINT-OPERATION-TRACK-PRINT-STATUS
                         "track-print-status" "gboolean" T T)
                        (UNIT GTK-PRINT-OPERATION-UNIT "unit" "GtkUnit" T T)
                        (USE-FULL-PAGE GTK-PRINT-OPERATION-USE-FULL-PAGE
                         "use-full-page" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkPrintOperation"))))

;;; --- Access Properties ------------------------------------------------------

(test print-operation-properties
  (let ((object (make-instance 'gtk:print-operation)))
    ;; allow-async
    (is-false (gtk:print-operation-allow-async object))
    (is-true (setf (gtk:print-operation-allow-async object) t))
    (is-true (gtk:print-operation-allow-async object))
    ;; current-page
    (is (= -1 (gtk:print-operation-current-page object)))
    ;; set n-pages > current-page
    (is (= 20 (setf (gtk:print-operation-n-pages object) 20)))
    (is (= 10 (setf (gtk:print-operation-current-page object) 10)))
    (is (= 10 (gtk:print-operation-current-page object)))
    ;; custom-tab-label
    (is-false (gtk:print-operation-custom-tab-label object))
    (is (string= "Custom Tab"
                 (setf (gtk:print-operation-custom-tab-label object)
                       "Custom Tab")))
    (is (string= "Custom Tab" (gtk:print-operation-custom-tab-label object)))
    ;; default-page-setup
    (is-false (gtk:print-operation-default-page-setup object))
    (is (typep (setf (gtk:print-operation-default-page-setup object)
                     (gtk:page-setup-new))
               'gtk:page-setup))
    (is (typep (gtk:print-operation-default-page-setup object) 'gtk:page-setup))
    ;; embed-page-setup
    (is-false (gtk:print-operation-embed-page-setup object))
    (is-true (setf (gtk:print-operation-embed-page-setup object) t))
    (is-true (gtk:print-operation-embed-page-setup object))
    ;; export-filename
    (is-false (gtk:print-operation-export-filename object))
    (is (string= "Export Filename"
                 (setf (gtk:print-operation-export-filename object)
                       "Export Filename")))
    (is (string= "Export Filename"
                 (gtk:print-operation-export-filename object)))
    ;; has-selection
    (is-false (gtk:print-operation-has-selection object))
    (is-true (setf (gtk:print-operation-has-selection object) t))
    (is-true (gtk:print-operation-has-selection object))
    ;; Set job-name
    (is (stringp (gtk:print-operation-job-name object)))
    (is (string= "Print Job"
                 (setf (gtk:print-operation-job-name object) "Print Job")))
    (is (string= "Print Job" (gtk:print-operation-job-name object)))
    ;; n-pages
    (is (= 20 (gtk:print-operation-n-pages object)))
    (is (= 30 (setf (gtk:print-operation-n-pages object) 30)))
    (is (= 30 (gtk:print-operation-n-pages object)))
    ;; n-pages-to-print
    (is (= -1 (gtk:print-operation-n-pages-to-print object)))
    ;; n-pages-to-print is not writeable
    (signals (error) (setf (gtk:print-operation-n-pages-to-print object) 10))
    (is (= -1 (gtk:print-operation-n-pages-to-print object)))
    ;; print-settings
    (is-false (gtk:print-operation-print-settings object))
    (is (typep (setf (gtk:print-operation-print-settings object)
                     (gtk:print-settings-new))
               'gtk:print-settings))
    (is (typep (gtk:print-operation-print-settings object) 'gtk:print-settings))
    ;; show-progess
    (is-false (gtk:print-operation-show-progress object))
    (is-true (setf (gtk:print-operation-show-progress object) t))
    (is-true (gtk:print-operation-show-progress object))
    ;; status
    (is (eq :initial (gtk:print-operation-status object)))
    ;; status is not writeable
    (signals (error) (setf (gtk:print-operation-status object) :preparing))
    (is (eq :initial (gtk:print-operation-status object)))
    ;; status-string
    (is (string= "" (gtk:print-operation-status-string object)))
    ;; status-string is not writeable
    (signals (error) (setf (gtk:print-operation-status-string object) "status"))
    (is (string= ""  (gtk:print-operation-status-string object)))
    ;; support-selection
    (is-false (gtk:print-operation-support-selection object))
    (is-true (setf (gtk:print-operation-support-selection object) t))
    (is-true (gtk:print-operation-support-selection object))
    ;; track-print-status
    (is-false (gtk:print-operation-track-print-status object))
    (is-true (setf (gtk:print-operation-track-print-status object) t))
    (is-true (gtk:print-operation-track-print-status object))
    ;; unit
    (is (eq :pixel (gtk:print-operation-unit object)))
    (is (eq :mm (setf (gtk:print-operation-unit object) :mm)))
    (is (eq :mm (gtk:print-operation-unit object)))
    ;; use-full-page
    (is-false (gtk:print-operation-use-full-page object))
    (is-true (setf (gtk:print-operation-use-full-page object) t))
    (is-true (gtk:print-operation-use-full-page object))))

;;;     gtk_print_operation_new

(test print-operation-new
  (is (eq 'gtk:print-operation (type-of (gtk:print-operation-new)))))

;;;     gtk_print_operation_get_error

;;;     gtk_print_operation_run

;; TODO: Fails in a second run of the testsuite with memory error. Check this.

;(test gtk-print-operation-run
;  (let ((operation (gtk-print-operation-new)))
;    (is (eq :cancel (gtk-print-operation-run operation :print nil)))))

;;;     gtk_print_operation_cancel
;;;     gtk_print_operation_draw_page_finish
;;;     gtk_print_operation_set_defer_drawing
;;;     gtk_print_operation_is_finished
;;;     gtk_print_run_page_setup_dialog
;;;     gtk_print_run_page_setup_dialog_async
;;;     gtk_print_operation_preview_end_preview
;;;     gtk_print_operation_preview_is_selected
;;;     gtk_print_operation_preview_render_page

;;; --- 2023-6-11 --------------------------------------------------------------
