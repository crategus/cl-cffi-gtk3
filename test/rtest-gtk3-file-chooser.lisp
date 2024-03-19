(in-package :gtk-test)

(def-suite gtk-file-chooser :in gtk-suite)
(in-suite gtk-file-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserAction

(test gtk-file-chooser-action
  ;; Check the type
  (is (g:type-is-enum "GtkFileChooserAction"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserAction")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_action_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser-action
          (glib:symbol-for-gtype "GtkFileChooserAction")))
  ;; Check the names
  (is (equal '("GTK_FILE_CHOOSER_ACTION_OPEN" "GTK_FILE_CHOOSER_ACTION_SAVE"
               "GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER"
               "GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER")
             (list-enum-item-name "GtkFileChooserAction")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkFileChooserAction")))
  ;; Check the nick names
  (is (equal '("open" "save" "select-folder" "create-folder")
             (list-enum-item-nick "GtkFileChooserAction")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkFileChooserAction"
                             GTK-FILE-CHOOSER-ACTION
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_file_chooser_action_get_type")
                             (:OPEN 0)
                             (:SAVE 1)
                             (:SELECT-FOLDER 2)
                             (:CREATE-FOLDER 3))
             (gobject:get-g-type-definition "GtkFileChooserAction"))))

;;;     GtkFileChooserConfirmation

(test gtk-file-chooser-confirmation
  ;; Check the type
  (is (g:type-is-enum "GtkFileChooserConfirmation"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserConfirmation")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_confirmation_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser-confirmation
          (glib:symbol-for-gtype "GtkFileChooserConfirmation")))
  ;; Check the names
  (is (equal '("GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM"
               "GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME"
               "GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN")
             (list-enum-item-name "GtkFileChooserConfirmation")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkFileChooserConfirmation")))
  ;; Check the nick names
  (is (equal '("confirm" "accept-filename" "select-again")
             (list-enum-item-nick "GtkFileChooserConfirmation")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkFileChooserConfirmation"
                             GTK-FILE-CHOOSER-CONFIRMATION
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_file_chooser_confirmation_get_type")
                             (:CONFIRM 0)
                             (:ACCEPT-FILENAME 1)
                             (:SELECT-AGAIN 2))
             (gobject:get-g-type-definition "GtkFileChooserConfirmation"))))

;;;     GtkFileChooser

(test gtk-file-chooser-interface
  ;; Type check
  (is (g:type-is-interface "GtkFileChooser"))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser
          (glib:symbol-for-gtype "GtkFileChooser")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooser")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("action" "create-folders" "do-overwrite-confirmation"
               "extra-widget" "filter" "local-only" "preview-widget"
               "preview-widget-active" "select-multiple" "show-hidden"
               "use-preview-label")
             (list-interface-properties "GtkFileChooser")))
  ;; Check the signals
  (is (equal '("confirm-overwrite" "current-folder-changed" "file-activated"
               "selection-changed" "update-preview")
             (list-signals "GtkFileChooser")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkFileChooser" GTK-FILE-CHOOSER
                    (:EXPORT T :TYPE-INITIALIZER "gtk_file_chooser_get_type")
                    (ACTION GTK-FILE-CHOOSER-ACTION "action"
                     "GtkFileChooserAction" T T)
                    (CREATE-FOLDERS GTK-FILE-CHOOSER-CREATE-FOLDERS
                     "create-folders" "gboolean" T T)
                    (DO-OVERWRITE-CONFIRMATION
                     GTK-FILE-CHOOSER-DO-OVERWRITE-CONFIRMATION
                     "do-overwrite-confirmation" "gboolean" T T)
                    (EXTRA-WIDGET GTK-FILE-CHOOSER-EXTRA-WIDGET "extra-widget"
                     "GtkWidget" T T)
                    (FILTER GTK-FILE-CHOOSER-FILTER "filter" "GtkFileFilter" T
                     T)
                    (LOCAL-ONLY GTK-FILE-CHOOSER-LOCAL-ONLY "local-only"
                     "gboolean" T T)
                    (PREVIEW-WIDGET GTK-FILE-CHOOSER-PREVIEW-WIDGET
                     "preview-widget" "GtkWidget" T T)
                    (PREVIEW-WIDGET-ACTIVE
                     GTK-FILE-CHOOSER-PREVIEW-WIDGET-ACTIVE
                     "preview-widget-active" "gboolean" T T)
                    (SELECT-MULTIPLE GTK-FILE-CHOOSER-SELECT-MULTIPLE
                     "select-multiple" "gboolean" T T)
                    (SHOW-HIDDEN GTK-FILE-CHOOSER-SHOW-HIDDEN "show-hidden"
                     "gboolean" T T)
                    (USE-PREVIEW-LABEL GTK-FILE-CHOOSER-USE-PREVIEW-LABEL
                     "use-preview-label" "gboolean" T T))
             (gobject:get-g-type-definition "GtkFileChooser"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-file-chooser-properties
  (let ((chooser (make-instance 'gtk:file-chooser-widget)))
    (is (eq :open (gtk:file-chooser-action chooser)))
    (is-true (gtk:file-chooser-create-folders chooser))
    (is-false (gtk:file-chooser-do-overwrite-confirmation chooser))
    (is-false (gtk:file-chooser-extra-widget chooser))
    (is-false (gtk:file-chooser-filter chooser))
    (is-true (gtk:file-chooser-local-only chooser))
    (is-false (gtk:file-chooser-preview-widget chooser))
    (is-true (gtk:file-chooser-preview-widget-active chooser))
    (is-false (gtk:file-chooser-select-multiple chooser))
    (is-false (gtk:file-chooser-show-hidden chooser))
    (is-true (gtk:file-chooser-use-preview-label chooser))))

;;; --- Signals ----------------------------------------------------------------

;;;     confirm-overwrite

(test gtk-file-chooser-confirm-overwrite-signal
  (let ((query (g:signal-query (g:signal-lookup "confirm-overwrite"
                                                "GtkFileChooser"))))
    (is (string= "confirm-overwrite" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooser"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "GtkFileChooserConfirmation"
                 (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     current-folder-changed

(test gtk-file-chooser-current-folder-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "current-folder-changed"
                                                "GtkFileChooser"))))
    (is (string= "current-folder-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooser"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void"
                 (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     file-activated

(test gtk-file-chooser-file-activated-signal
  (let ((query (g:signal-query (g:signal-lookup "file-activated"
                                                "GtkFileChooser"))))
    (is (string= "file-activated" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooser"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void"
                 (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     selection-changed

(test gtk-file-chooser-selection-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "selection-changed"
                                                "GtkFileChooser"))))
    (is (string= "selection-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooser"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void"
                 (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     update-preview

(test gtk-file-chooser-update-preview-signal
  (let ((query (g:signal-query (g:signal-lookup "update-preview"
                                                "GtkFileChooser"))))
    (is (string= "update-preview" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooser"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void"
                 (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_set_current_name
;;;     gtk_file_chooser_get_current_name

(test gtk-file-chooser-current-name
  (let ((chooser (make-instance 'gtk:file-chooser-widget
                                :action :create-folder)))
    (is (string= "" (gtk:file-chooser-current-name chooser)))
    (is (string= "Untitled"
                 (setf (gtk:file-chooser-current-name chooser) "Untitled")))
    (is (string= "Untitled" (gtk:file-chooser-current-name chooser)))))

;;;     gtk_file_chooser_get_filename
;;;     gtk_file_chooser_set_filename

(test gtk-file-chooser-filename
  (let ((filename "/home/dieter/Lisp/lisp-projects/cl-gtk/test/ducky.png")
        (chooser (make-instance 'gtk:file-chooser-widget
                                :action :save)))
    (is-false (gtk:file-chooser-filename chooser))
    (is (string= filename
                 (setf (gtk:file-chooser-filename chooser) filename)))
    (is-false (gtk:file-chooser-filename chooser))))

;;;     gtk_file_chooser_select_filename
;;;     gtk_file_chooser_unselect_filename
;;;     gtk_file_chooser_select_all
;;;     gtk_file_chooser_unselect_all
;;;     gtk_file_chooser_get_filenames

;;;     gtk_file_chooser_set_current_folder
;;;     gtk_file_chooser_get_current_folder

(test gtk-file-chooser-current-folder
  (let ((filename "/home/dieter/Lisp/lisp-projects/cl-gtk/test")
        (chooser (make-instance 'gtk:file-chooser-widget
                                :action :save)))
    (is-false (gtk:file-chooser-current-folder chooser))
    (is (string= filename
                 (setf (gtk:file-chooser-current-folder chooser) filename)))
    (is-false (gtk:file-chooser-current-folder chooser))))

;;;     gtk_file_chooser_get_uri
;;;     gtk_file_chooser_set_uri
;;;     gtk_file_chooser_select_uri
;;;     gtk_file_chooser_unselect_uri
;;;     gtk_file_chooser_get_uris
;;;     gtk_file_chooser_set_current_folder_uri
;;;     gtk_file_chooser_get_current_folder_uri
;;;     gtk_file_chooser_get_preview_filename
;;;     gtk_file_chooser_get_preview_uri
;;;     gtk_file_chooser_add_filter
;;;     gtk_file_chooser_remove_filter
;;;     gtk_file_chooser_list_filters

;;;     gtk_file_chooser_add_shortcut_folder
;;;     gtk_file_chooser_remove_shortcut_folder
;;;     gtk_file_chooser_list_shortcut_folders

(test gtk-file-chooser-shortcut-folder
  (let ((chooser (make-instance 'gtk:file-chooser-widget)))
    (is (equal '() (gtk:file-chooser-list-shortcut-folders chooser)))
    (is-true (gtk:file-chooser-add-shortcut-folder chooser "unknown"))
    (is (every #'stringp
               (gtk:file-chooser-list-shortcut-folders chooser)))
    (is (= 1 (length (gtk:file-chooser-list-shortcut-folders chooser))))
    (is-true (gtk:file-chooser-remove-shortcut-folder chooser "unknown"))
    (is (equal '() (gtk:file-chooser-list-shortcut-folders chooser)))))

;;;     gtk_file_chooser_add_shortcut_folder_uri
;;;     gtk_file_chooser_remove_shortcut_folder_uri
;;;     gtk_file_chooser_list_shortcut_folder_uris

(test gtk-file-chooser-shortcut-folder-uri
  (let ((chooser (make-instance 'gtk:file-chooser-widget)))
    (is (equal '() (gtk:file-chooser-list-shortcut-folder-uris chooser)))
    (is-true (gtk:file-chooser-add-shortcut-folder-uri chooser "unknown"))
    (is (equal '("unknown")
               (gtk:file-chooser-list-shortcut-folder-uris chooser)))
    (is-true (gtk:file-chooser-remove-shortcut-folder-uri chooser "unknown"))
    (is (equal '() (gtk:file-chooser-list-shortcut-folder-uris chooser)))))

;;;     gtk_file_chooser_get_current_folder_file
;;;     gtk_file_chooser_get_file
;;;     gtk_file_chooser_get_files
;;;     gtk_file_chooser_get_preview_file
;;;     gtk_file_chooser_select_file
;;;     gtk_file_chooser_set_current_folder_file
;;;     gtk_file_chooser_set_file
;;;     gtk_file_chooser_unselect_file

;;; 2024-3-14
