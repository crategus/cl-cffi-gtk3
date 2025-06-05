(in-package :gtk-test)

(def-suite gtk-dialog :in gtk-suite)
(in-suite gtk-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDialogFlags

(test gtk-dialog-flags
  ;; Check type
  (is (g:type-is-flags "GtkDialogFlags"))
  ;; Check registered name
  (is (eq 'gtk:dialog-flags
          (glib:symbol-for-gtype "GtkDialogFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDialogFlags")
          (g:gtype (cffi:foreign-funcall "gtk_dialog_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_DIALOG_MODAL" "GTK_DIALOG_DESTROY_WITH_PARENT"
               "GTK_DIALOG_USE_HEADER_BAR")
             (glib-test:list-flags-item-names "GtkDialogFlags")))
  ;; Check values
  (is (equal '(1 2 4)
             (glib-test:list-flags-item-values "GtkDialogFlags")))
  ;; Check nick names
  (is (equal '("modal" "destroy-with-parent" "use-header-bar")
             (glib-test:list-flags-item-nicks "GtkDialogFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkDialogFlags" GTK:DIALOG-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_dialog_flags_get_type")
                                     (:MODAL 1)
                                     (:DESTROY-WITH-PARENT 2)
                                     (:USE-HEADER-BAR 4))
             (gobject:get-gtype-definition "GtkDialogFlags"))))

;;;     GtkResponseType

(test gtk-response-type
  ;; Check type
  (is (g:type-is-enum "GtkResponseType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkResponseType")
          (g:gtype (cffi:foreign-funcall "gtk_response_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:response-type
          (glib:symbol-for-gtype "GtkResponseType")))
  ;; Check names
  (is (equal '("GTK_RESPONSE_NONE" "GTK_RESPONSE_REJECT" "GTK_RESPONSE_ACCEPT"
               "GTK_RESPONSE_DELETE_EVENT" "GTK_RESPONSE_OK"
               "GTK_RESPONSE_CANCEL" "GTK_RESPONSE_CLOSE" "GTK_RESPONSE_YES"
               "GTK_RESPONSE_NO" "GTK_RESPONSE_APPLY" "GTK_RESPONSE_HELP")
             (glib-test:list-enum-item-names "GtkResponseType")))
  ;; Check values
  (is (equal '(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11)
             (glib-test:list-enum-item-values "GtkResponseType")))
  ;; Check nick names
  (is (equal '("none" "reject" "accept" "delete-event" "ok" "cancel" "close"
               "yes" "no" "apply" "help")
             (glib-test:list-enum-item-nicks "GtkResponseType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkResponseType" GTK:RESPONSE-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_response_type_get_type")
                                    (:NONE -1)
                                    (:REJECT -2)
                                    (:ACCEPT -3)
                                    (:DELETE-EVENT -4)
                                    (:OK -5)
                                    (:CANCEL -6)
                                    (:CLOSE -7)
                                    (:YES -8)
                                    (:NO -9)
                                    (:APPLY -10)
                                    (:HELP -11))
             (gobject:get-gtype-definition "GtkResponseType"))))

;;;     GtkDialog

(test gtk-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkDialog"))
  ;; Check registered name
  (is (eq 'gtk:dialog
          (glib:symbol-for-gtype "GtkDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDialog")
          (g:gtype (cffi:foreign-funcall "gtk_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkDialog")))
  ;; Check children
  #-windows
  (is (equal '("GtkAboutDialog" "GtkAppChooserDialog" "GtkColorChooserDialog"
               "GtkColorSelectionDialog" "GtkFileChooserDialog"
               "GtkFontChooserDialog" "GtkMessageDialog"
               "GtkPageSetupUnixDialog" "GtkPrintUnixDialog"
               "GtkRecentChooserDialog")
             (glib-test:list-children "GtkDialog")))
  #+windows
  (is (equal '("GtkAboutDialog" "GtkAppChooserDialog" "GtkColorChooserDialog"
               "GtkColorSelectionDialog" "GtkFileChooserDialog"
               "GtkFontChooserDialog"  "GtkMessageDialog"
               "GtkRecentChooserDialog")
             (glib-test:list-children "GtkDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkDialog")))
  ;; Check class properties
  (is (equal '("use-header-bar")
             (glib-test:list-properties "GtkDialog")))
  ;; Check style properties
  (is (equal '("action-area-border" "button-spacing" "content-area-border"
               "content-area-spacing")
             (gtk-test:list-style-properties "GtkDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkDialog")))
  ;; Check the signals
  (is (equal '("close" "response")
             (glib-test:list-signals "GtkDialog")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDialog" GTK:DIALOG
                      (:SUPERCLASS GTK:WINDOW
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_dialog_get_type")
                      ((USE-HEADER-BAR DIALOG-USE-HEADER-BAR
                        "use-header-bar" "gint" T NIL)))
             (gobject:get-gtype-definition "GtkDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-dialog-properties.1
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:dialog))
    (is (= 0 (gtk:dialog-use-header-bar dialog)))
    (is-false (gtk:widget-destroy dialog))))

(test gtk-dialog-properties.2
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:dialog :use-header-bar 1))
    (is (= 1 (gtk:dialog-use-header-bar dialog)))
    (is-false (gtk:widget-destroy dialog))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-dialog-style-properties
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:dialog))
    (is (= 0 (gtk:widget-style-property dialog "action-area-border")))
    (is (= 4 (gtk:widget-style-property dialog "button-spacing")))
    (is (= 2 (gtk:widget-style-property dialog "content-area-border")))
    (is (= 0 (gtk:widget-style-property dialog "content-area-spacing")))
    (is-false (gtk:widget-destroy dialog))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-dialog-close-signal
  (let* ((name "close")
         (gtype (g:gtype "GtkDialog"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gtk-dialog-response-signal
  (let* ((name "response")
         (gtype (g:gtype "GtkDialog"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_dialog_new

(test gtk-dialog-new
  (glib-test:with-check-memory (dialog)
    (is (typep (setf dialog (gtk:dialog-new)) 'gtk:dialog))
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_new-with-buttons

(test gtk-dialog-new-with-buttons
  (glib-test:with-check-memory (dialog)
    (is (typep (setf dialog
                     (gtk:dialog-new-with-buttons "title"
                                                  nil
                                                  :modal
                                                  "OK" 1
                                                  "Cancel" 2))
               'gtk:dialog))
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_run
;;;     gtk_dialog_response

;;;     gtk_dialog_add_button

(test gtk-dialog-add-button
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:dialog))
    ;; No button in the action area
    (is (= 0 (length (gtk:container-children (gtk:dialog-action-area dialog)))))
    (is (equal '() (gtk:container-children (gtk:dialog-action-area dialog))))
    ;; Add first button to the action area
    (is (typep (gtk:dialog-add-button dialog "button1" 1) 'gtk:button))
    (is (= 1 (length (gtk:container-children (gtk:dialog-action-area dialog)))))
    (is (every #'(lambda (x) (typep x 'gtk:button))
               (gtk:container-children (gtk:dialog-action-area dialog))))
    ;; Add another button to the action area
    (is (typep (gtk:dialog-add-button dialog "button2" 2) 'gtk:button))
    (is (= 2 (length (gtk:container-children (gtk:dialog-action-area dialog)))))
    (is (every #'(lambda (x) (typep x 'gtk:button))
               (gtk:container-children (gtk:dialog-action-area dialog))))
    ;; Remove references
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_add_buttons

(test gtk-dialog-add-buttons
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:dialog))
    ;; No button in the action area
    (is (= 0 (length (gtk:container-children (gtk:dialog-action-area dialog)))))
    (is (equal '() (gtk:container-children (gtk:dialog-action-area dialog))))
    ;; Add two buttons
    (is-false (gtk:dialog-add-buttons dialog "button1" 1 "button2" 2))
    (is (= 2 (length (gtk:container-children (gtk:dialog-action-area dialog)))))
    (is (every #'(lambda (x) (typep x 'gtk:button))
               (gtk:container-children (gtk:dialog-action-area dialog))))
    ;; Remove references
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_add_action_widget

(test gtk-dialog-add-action-widget
  (glib-test:with-check-memory (dialog widget)
    (setf dialog (make-instance 'gtk:dialog))
    (setf widget (make-instance 'gtk:button :label "BUTTON"))
    (is-false (gtk:dialog-add-action-widget dialog widget 1))
    (is (member widget
               (gtk:container-children (gtk:dialog-action-area dialog))
               :test #'eq))
    ;; Remove references
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_set_default_response

(test gtk-dialog-set-default-response
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:dialog))
    (is-false (gtk:dialog-add-buttons dialog "button1" 1 "button2" 2))
    (is-false (gtk:dialog-set-default-response dialog 2))
    ;; Remove references
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_set_response_sensitive

(test gtk-dialog-set-response-sensitive
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:dialog))
    (is-false (gtk:dialog-add-buttons dialog "button1" 1 "button2" 2))
    (is-false (gtk:dialog-set-response-sensitive dialog 1 t))
    (is-false (gtk:dialog-set-response-sensitive dialog 2 nil))
    (let ((buttons (gtk:container-children (gtk:dialog-action-area dialog))))
      (is-true (gtk:widget-sensitive (first buttons)))
      (is-false (gtk:widget-sensitive (second buttons))))
    ;; Remove references
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response

;;;     gtk_dialog_action_area

(test gtk-dialog-action-area
  (glib-test:with-check-memory (dialog)
    (setf dialog
          (gtk:dialog-new-with-buttons "title"
                                       nil :modal "OK" 1 "Cancel" 2))
    (is (typep (gtk:dialog-action-area dialog) 'gtk:button-box))
    (is (every (lambda (x) (typep x 'gtk:button))
               (gtk:container-children (gtk:dialog-action-area dialog))))
    ;; Remove references
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_content_area

(test gtk-dialog-content-area
  (glib-test:with-check-memory (dialog)
    (setf dialog
          (gtk:dialog-new-with-buttons "title"
                                       nil :modal "OK" 1 "Cancel" 2))
    (is (typep (gtk:dialog-content-area dialog) 'gtk:box))
    (is (eq :vertical
            (gtk:orientable-orientation (gtk:dialog-content-area dialog))))
    ;; Remove references
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_dialog_get_header_bar
;;;     gtk_alternative_dialog_button_order
;;;     gtk_dialog_set_alternative_button_order
;;;     gtk_dialog_set_alternative_button_order_from_array

;;; 2025-06-05
