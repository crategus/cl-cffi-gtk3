(in-package :gtk-test)

(def-suite gtk-file-chooser :in gtk-suite)
(in-suite gtk-file-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserAction

(test gtk-file-chooser-action
  ;; Check type
  (is (g:type-is-enum "GtkFileChooserAction"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooserAction")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_action_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:file-chooser-action
          (glib:symbol-for-gtype "GtkFileChooserAction")))
  ;; Check names
  (is (equal '("GTK_FILE_CHOOSER_ACTION_OPEN" "GTK_FILE_CHOOSER_ACTION_SAVE"
               "GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER"
               "GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER")
             (glib-test:list-enum-item-names "GtkFileChooserAction")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkFileChooserAction")))
  ;; Check nick names
  (is (equal '("open" "save" "select-folder" "create-folder")
             (glib-test:list-enum-item-nicks "GtkFileChooserAction")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkFileChooserAction"
                                    GTK:FILE-CHOOSER-ACTION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_file_chooser_action_get_type")
                       (:OPEN 0)
                       (:SAVE 1)
                       (:SELECT-FOLDER 2)
                       (:CREATE-FOLDER 3))
             (gobject:get-gtype-definition "GtkFileChooserAction"))))

;;;     GtkFileChooserConfirmation

(test gtk-file-chooser-confirmation
  ;; Check type
  (is (g:type-is-enum "GtkFileChooserConfirmation"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserConfirmation")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_confirmation_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:file-chooser-confirmation
          (glib:symbol-for-gtype "GtkFileChooserConfirmation")))
  ;; Check names
  (is (equal '("GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM"
               "GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME"
               "GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN")
             (glib-test:list-enum-item-names "GtkFileChooserConfirmation")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkFileChooserConfirmation")))
  ;; Check nick names
  (is (equal '("confirm" "accept-filename" "select-again")
             (glib-test:list-enum-item-nicks "GtkFileChooserConfirmation")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkFileChooserConfirmation"
                                    GTK:FILE-CHOOSER-CONFIRMATION
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_file_chooser_confirmation_get_type")
                       (:CONFIRM 0)
                       (:ACCEPT-FILENAME 1)
                       (:SELECT-AGAIN 2))
             (gobject:get-gtype-definition "GtkFileChooserConfirmation"))))

;;;     GtkFileChooser

(test gtk-file-chooser-interface
  ;; Check type
  (is (g:type-is-interface "GtkFileChooser"))
  ;; Check registered name
  (is (eq 'gtk:file-chooser
          (glib:symbol-for-gtype "GtkFileChooser")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooser")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_get_type" :size))))
  ;; Check interface properties
  (is (equal '("action" "create-folders" "do-overwrite-confirmation"
               "extra-widget" "filter" "local-only" "preview-widget"
               "preview-widget-active" "select-multiple" "show-hidden"
               "use-preview-label")
             (glib-test:list-interface-properties "GtkFileChooser")))
  ;; Check signals
  (is (equal '("confirm-overwrite" "current-folder-changed" "file-activated"
               "selection-changed" "update-preview")
             (glib-test:list-signals "GtkFileChooser")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkFileChooser" GTK:FILE-CHOOSER
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_file_chooser_get_type")
                       (ACTION FILE-CHOOSER-ACTION
                        "action" "GtkFileChooserAction" T T)
                       (CREATE-FOLDERS FILE-CHOOSER-CREATE-FOLDERS
                        "create-folders" "gboolean" T T)
                       (DO-OVERWRITE-CONFIRMATION
                        FILE-CHOOSER-DO-OVERWRITE-CONFIRMATION
                        "do-overwrite-confirmation" "gboolean" T T)
                       (EXTRA-WIDGET FILE-CHOOSER-EXTRA-WIDGET
                        "extra-widget" "GtkWidget" T T)
                       (FILTER FILE-CHOOSER-FILTER "filter" "GtkFileFilter" T T)
                       (LOCAL-ONLY FILE-CHOOSER-LOCAL-ONLY
                        "local-only" "gboolean" T T)
                       (PREVIEW-WIDGET FILE-CHOOSER-PREVIEW-WIDGET
                        "preview-widget" "GtkWidget" T T)
                       (PREVIEW-WIDGET-ACTIVE FILE-CHOOSER-PREVIEW-WIDGET-ACTIVE
                        "preview-widget-active" "gboolean" T T)
                       (SELECT-MULTIPLE FILE-CHOOSER-SELECT-MULTIPLE
                        "select-multiple" "gboolean" T T)
                       (SHOW-HIDDEN FILE-CHOOSER-SHOW-HIDDEN
                        "show-hidden" "gboolean" T T)
                       (USE-PREVIEW-LABEL FILE-CHOOSER-USE-PREVIEW-LABEL
                        "use-preview-label" "gboolean" T T))
             (gobject:get-gtype-definition "GtkFileChooser"))))

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

;;; 2024-9-23
