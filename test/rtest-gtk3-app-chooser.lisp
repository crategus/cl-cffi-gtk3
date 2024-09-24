(in-package :gtk-test)

(def-suite gtk-app-chooser :in gtk-suite)
(in-suite gtk-app-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooser

(test gtk-app-chooser-interface
  ;; Check type
  (is-true (g:type-is-interface "GtkAppChooser"))
  ;; Check registered name
  (is (eq 'gtk:app-chooser
          (glib:symbol-for-gtype "GtkAppChooser")))
  ;; Check interface properties
  (is (equal '("content-type")
             (glib-test:list-interface-properties "GtkAppChooser")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkAppChooser" GTK:APP-CHOOSER
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_app_chooser_get_type")
                       (CONTENT-TYPE APP-CHOOSER-CONTENT-TYPE
                        "content-type" "gchararray" T NIL))
             (gobject:get-gtype-definition "GtkAppChooser"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-properties
  (let ((chooser (make-instance 'gtk:app-chooser-button)))
    (is-false (gtk:app-chooser-content-type chooser)))
  (let ((chooser (make-instance 'gtk:app-chooser-button
                                :content-type "plain/text")))
    (is (string= "plain/text" (gtk:app-chooser-content-type chooser)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_get_app_info

(test gtk-app-chooser-app-info
  (let ((chooser (make-instance 'gtk:app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk:app-chooser-app-info chooser))))

;;;     gtk_app_chooser_refresh

(test gtk-app-chooser-refresh
  (let ((chooser (make-instance 'gtk:app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk:app-chooser-refresh chooser))))

;;; 2024-9-23
