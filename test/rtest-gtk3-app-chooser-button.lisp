(in-package :gtk-test)

(def-suite gtk-app-chooser-button :in gtk-suite)
(in-suite gtk-app-chooser-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserButton

(test app-chooser-button-class
  ;; Type check
  (is (g:type-is-object "GtkAppChooserButton"))
  ;; Check the registered name
  (is (eq 'gtk:app-chooser-button
          (glib:symbol-for-gtype "GtkAppChooserButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAppChooserButton")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkComboBox") (g:type-parent "GtkAppChooserButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAppChooserButton")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkCellLayout" "GtkCellEditable"
               "GtkAppChooser")
             (list-interfaces "GtkAppChooserButton")))
  ;; Check the class properties
  (is (equal '("content-type" "heading" "show-default-item" "show-dialog-item")
             (list-properties "GtkAppChooserButton")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkAppChooserButton")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkAppChooserButton")))
  ;; Check the signals
  (is (equal '("custom-item-activated")
             (list-signals "GtkAppChooserButton")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAppChooserButton" 
                                             GTK-APP-CHOOSER-BUTTON
                       (:SUPERCLASS GTK-COMBO-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkAppChooser" "GtkBuildable"
                         "GtkCellEditable" "GtkCellLayout")
                        :TYPE-INITIALIZER "gtk_app_chooser_button_get_type")
                       ((HEADING GTK-APP-CHOOSER-BUTTON-HEADING "heading"
                         "gchararray" T T)
                        (SHOW-DEFAULT-ITEM
                         GTK-APP-CHOOSER-BUTTON-SHOW-DEFAULT-ITEM
                         "show-default-item" "gboolean" T T)
                        (SHOW-DIALOG-ITEM
                         GTK-APP-CHOOSER-BUTTON-SHOW-DIALOG-ITEM
                         "show-dialog-item" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkAppChooserButton"))))

;;; --- Properties -------------------------------------------------------------

(test app-chooser-button-properties
  (let ((chooser (make-instance 'gtk:app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk:app-chooser-button-heading chooser))
    (is-false (gtk:app-chooser-button-show-default-item chooser))
    (is-false (gtk:app-chooser-button-show-dialog-item chooser))))

;;; --- Signals ----------------------------------------------------------------

;;;         void    custom-item-activated     Has Details

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_button_new

(test app-chooser-button-new
  (is (eq 'gtk:app-chooser-button (type-of (gtk:app-chooser-button-new nil))))
  (is (eq 'gtk:app-chooser-button (type-of (gtk:app-chooser-button-new (cffi:null-pointer)))))
  (is (eq 'gtk:app-chooser-button (type-of (gtk:app-chooser-button-new "plain/text")))))

;;;     gtk_app_chooser_button_append_custom_item
;;;     gtk_app_chooser_button_append_separator
;;;     gtk_app_chooser_button_set_active_custom_item

;;; --- 2023-5-29 --------------------------------------------------------------
