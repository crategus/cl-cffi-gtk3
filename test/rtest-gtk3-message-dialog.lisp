(in-package :gtk-test)

(def-suite gtk-message-dialog :in gtk-suite)
(in-suite gtk-message-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMessageType

(test gtk-message-type
  ;; Check type
  (is (g:type-is-enum "GtkMessageType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMessageType")
          (g:gtype (cffi:foreign-funcall "gtk_message_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:message-type
          (glib:symbol-for-gtype "GtkMessageType")))
  ;; Check names
  (is (equal '("GTK_MESSAGE_INFO" "GTK_MESSAGE_WARNING" "GTK_MESSAGE_QUESTION"
               "GTK_MESSAGE_ERROR" "GTK_MESSAGE_OTHER")
             (glib-test:list-enum-item-names "GtkMessageType")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (glib-test:list-enum-item-values "GtkMessageType")))
  ;; Check nick names
  (is (equal '("info" "warning" "question" "error" "other")
             (glib-test:list-enum-item-nicks "GtkMessageType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkMessageType" GTK:MESSAGE-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_message_type_get_type")
                                    (:INFO 0)
                                    (:WARNING 1)
                                    (:QUESTION 2)
                                    (:ERROR 3)
                                    (:OTHER 4))
             (gobject:get-gtype-definition "GtkMessageType"))))

;;;     GtkButtonsType

(test gtk-buttons-type
  ;; Check type
  (is (g:type-is-enum "GtkButtonsType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkButtonsType")
          (g:gtype (cffi:foreign-funcall "gtk_buttons_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:buttons-type
          (glib:symbol-for-gtype "GtkButtonsType")))
  ;; Check names
  (is (equal '("GTK_BUTTONS_NONE" "GTK_BUTTONS_OK" "GTK_BUTTONS_CLOSE"
               "GTK_BUTTONS_CANCEL" "GTK_BUTTONS_YES_NO"
               "GTK_BUTTONS_OK_CANCEL")
             (glib-test:list-enum-item-names "GtkButtonsType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GtkButtonsType")))
  ;; Check nick names
  (is (equal '("none" "ok" "close" "cancel" "yes-no" "ok-cancel")
             (glib-test:list-enum-item-nicks "GtkButtonsType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkButtonsType" GTK:BUTTONS-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_buttons_type_get_type")
                                    (:NONE 0)
                                    (:OK 1)
                                    (:CLOSE 2)
                                    (:CANCEL 3)
                                    (:YES-NO 4)
                                    (:OK-CANCEL 5))
             (gobject:get-gtype-definition "GtkButtonsType"))))

;;;     GtkMessageDialog

(test gtk-message-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkMessageDialog"))
  ;; Check registered name
  (is (eq 'gtk:message-dialog
          (glib:symbol-for-gtype "GtkMessageDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMessageDialog")
          (g:gtype (cffi:foreign-funcall "gtk_message_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkMessageDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMessageDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkMessageDialog")))
  ;; Check class properties
  (is (equal '("buttons" "image" "message-area" "message-type" "secondary-text"
               "secondary-use-markup" "text" "use-markup")
             (glib-test:list-properties "GtkMessageDialog")))
  ;; Check style properties
  (is (equal '("message-border")
             (gtk-test:list-style-properties "GtkMessageDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkMessageDialog")))
  (is (equal '()
             (glib-test:list-signals "GtkMessageDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMessageDialog" GTK:MESSAGE-DIALOG
                      (:SUPERCLASS GTK:DIALOG
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_message_dialog_get_type")
                      ((BUTTONS MESSAGE-DIALOG-BUTTONS
                        "buttons" "GtkButtonsType" NIL NIL)
                       (IMAGE MESSAGE-DIALOG-IMAGE "image" "GtkWidget" T T)
                       (MESSAGE-AREA MESSAGE-DIALOG-MESSAGE-AREA
                        "message-area" "GtkWidget" T NIL)
                       (MESSAGE-TYPE MESSAGE-DIALOG-MESSAGE-TYPE
                        "message-type" "GtkMessageType" T T)
                       (SECONDARY-TEXT MESSAGE-DIALOG-SECONDARY-TEXT
                        "secondary-text" "gchararray" T T)
                       (SECONDARY-USE-MARKUP
                        MESSAGE-DIALOG-SECONDARY-USE-MARKUP
                        "secondary-use-markup" "gboolean" T T)
                       (TEXT MESSAGE-DIALOG-TEXT "text" "gchararray" T T)
                       (USE-MARKUP MESSAGE-DIALOG-USE-MARKUP
                        "use-markup" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkMessageDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-message-dialog-properties
  (glib-test:with-check-memory (dialog)
    (is (typep (setf dialog (make-instance 'gtk:message-dialog)) 'gtk:dialog))
    ;; Property buttons is not readable
    (signals (error) (gtk:message-dialog-buttons dialog))
    #-windows
    (is (typep (gtk:message-dialog-image dialog) 'gtk:image))
    (is (typep (gtk:message-dialog-message-area dialog) 'gtk:box))
    (is (eq :info (gtk:message-dialog-message-type dialog)))
    (is-false (gtk:message-dialog-secondary-text dialog))
    (is-false (gtk:message-dialog-secondary-use-markup dialog))
    (is (string= "" (gtk:message-dialog-text dialog)))
    (is-false (gtk:message-dialog-use-markup dialog))
    ;; Remove references
    (is-false (gtk:widget-destroy dialog))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-message-dialog-style-properties
  (glib-test:with-check-memory (dialog)
    (is (typep (setf dialog (make-instance 'gtk:message-dialog)) 'gtk:dialog))
    (is (= 12
           (gtk:widget-style-property dialog "message-border")))
    (is-false (gtk:widget-destroy dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_message_dialog_new

(test gtk-message-dialog-new
  (glib-test:with-check-memory (dialog)
    (is (typep (setf dialog
                     (gtk:message-dialog-new nil
                                             '(:modal :use-header-bar)
                                             :question
                                             :ok-cancel
                                             "The ~a value."
                                             10))
               'gtk:dialog))
    #-windows
    (is (typep (gtk:message-dialog-image dialog) 'gtk:image))
    (is (typep (gtk:message-dialog-message-area dialog) 'gtk:box))
    (is (eq :question (gtk:message-dialog-message-type dialog)))
    (is-false (gtk:message-dialog-secondary-text dialog))
    (is-false (gtk:message-dialog-secondary-use-markup dialog))
    (is (string= "The 10 value." (gtk:message-dialog-text dialog)))
    (is-false (gtk:message-dialog-use-markup dialog))
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_message_dialog_new_with_markup

(test gtk-message-dialog-new-with-markup
  (glib-test:with-check-memory (dialog)
    (is (typep (setf dialog
                     (gtk:message-dialog-new-with-markup nil
                                                         '(:modal :use-header-bar)
                                                         :question
                                                         :ok-cancel
                                                         "<b>The ~a value.</b>"
                                                         10))
               'gtk:dialog))
    #-windows
    (is (typep (gtk:message-dialog-image dialog) 'gtk:image))
    (is (typep (gtk:message-dialog-message-area dialog) 'gtk:box))
    (is (eq :question (gtk:message-dialog-message-type dialog)))
    (is-false (gtk:message-dialog-secondary-text dialog))
    (is-false (gtk:message-dialog-secondary-use-markup dialog))
    (is (string= "<b>The 10 value.</b>" (gtk:message-dialog-text dialog)))
    (is-true (gtk:message-dialog-use-markup dialog))
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_message_dialog_set_markup
;;;     gtk_message_dialog_format_secondary_text
;;;     gtk_message_dialog_format_secondary_markup

;;; 2025-06-05
