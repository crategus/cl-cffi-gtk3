(in-package :gtk-test)

(def-suite gtk-app-chooser-button :in gtk-suite)
(in-suite gtk-app-chooser-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserButton

(test gtk-app-chooser-button-class
  ;; Check type
  (is (g:type-is-object "GtkAppChooserButton"))
  ;; Check registered name
  (is (eq 'gtk:app-chooser-button
          (glib:symbol-for-gtype "GtkAppChooserButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAppChooserButton")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkComboBox") (g:type-parent "GtkAppChooserButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAppChooserButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkCellLayout" "GtkCellEditable"
               "GtkAppChooser")
             (glib-test:list-interfaces "GtkAppChooserButton")))
  ;; Check class properties
  (is (equal '("content-type" "heading" "show-default-item" "show-dialog-item")
             (glib-test:list-properties "GtkAppChooserButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkAppChooserButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkAppChooserButton")))
  ;; Check signals
  (is (equal '("custom-item-activated")
             (glib-test:list-signals "GtkAppChooserButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAppChooserButton"
                                      GTK:APP-CHOOSER-BUTTON
                       (:SUPERCLASS GTK:COMBO-BOX
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkAppChooser" "GtkBuildable"
                         "GtkCellEditable" "GtkCellLayout")
                        :TYPE-INITIALIZER "gtk_app_chooser_button_get_type")
                       ((HEADING  APP-CHOOSER-BUTTON-HEADING
                         "heading" "gchararray" T T)
                        (SHOW-DEFAULT-ITEM APP-CHOOSER-BUTTON-SHOW-DEFAULT-ITEM
                         "show-default-item" "gboolean" T T)
                        (SHOW-DIALOG-ITEM APP-CHOOSER-BUTTON-SHOW-DIALOG-ITEM
                         "show-dialog-item" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkAppChooserButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-button-properties
  (let ((chooser (make-instance 'gtk:app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk:app-chooser-button-heading chooser))
    (is-false (gtk:app-chooser-button-show-default-item chooser))
    (is-false (gtk:app-chooser-button-show-dialog-item chooser))))

;;; --- Signals ----------------------------------------------------------------

;;;         void    custom-item-activated     Has Details

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_button_new

(test gtk-app-chooser-button-new
  (is (eq 'gtk:app-chooser-button (type-of (gtk:app-chooser-button-new nil))))
  (is (eq 'gtk:app-chooser-button (type-of (gtk:app-chooser-button-new (cffi:null-pointer)))))
  (is (eq 'gtk:app-chooser-button (type-of (gtk:app-chooser-button-new "plain/text")))))

;;;     gtk_app_chooser_button_append_custom_item
;;;     gtk_app_chooser_button_append_separator
;;;     gtk_app_chooser_button_set_active_custom_item

;;; 2024-9-23
