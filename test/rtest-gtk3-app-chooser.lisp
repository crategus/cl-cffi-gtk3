(in-package :gtk-test)

(def-suite gtk-app-chooser :in gtk-suite)
(in-suite gtk-app-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooser

(test app-chooser-interface
  ;; Type check
  (is-true (g:type-is-interface "GtkAppChooser"))
  ;; Check the registered name
  (is (eq 'gtk:app-chooser
          (gobject:symbol-for-gtype "GtkAppChooser")))
  ;; Get the names of the interface properties.
  (is (equal '("content-type")
             (list-interface-properties "GtkAppChooser")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkAppChooser"
                                  GTK-APP-CHOOSER
                                  (:EXPORT T :TYPE-INITIALIZER
                                   "gtk_app_chooser_get_type")
                                  (CONTENT-TYPE GTK-APP-CHOOSER-CONTENT-TYPE
                                   "content-type" "gchararray" T NIL))
             (gobject:get-g-type-definition "GtkAppChooser"))))

;;; --- Properties -------------------------------------------------------------

(test app-chooser-properties
  (let ((chooser (make-instance 'gtk:app-chooser-button)))
    (is-false (gtk:app-chooser-content-type chooser)))
  (let ((chooser (make-instance 'gtk:app-chooser-button
                                :content-type "plain/text")))
    (is (string= "plain/text" (gtk:app-chooser-content-type chooser)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_get_app_info

(test app-chooser-app-info
  (let ((chooser (make-instance 'gtk:app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk:app-chooser-app-info chooser))))

;;;     gtk_app_chooser_refresh

(test app-chooser-refresh
  (let ((chooser (make-instance 'gtk:app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk:app-chooser-refresh chooser))))

;;; --- 2023-1-1 ---------------------------------------------------------------
