(in-package :gtk-test)

(def-suite gtk-model-button :in gtk-suite)
(in-suite gtk-model-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkButtonRole

(test gtk-button-role
  ;; Check type
  (is (g:type-is-enum "GtkButtonRole"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkButtonRole")
          (g:gtype (cffi:foreign-funcall "gtk_button_role_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:button-role
          (glib:symbol-for-gtype "GtkButtonRole")))
  ;; Check names
  (is (equal '("GTK_BUTTON_ROLE_NORMAL" "GTK_BUTTON_ROLE_CHECK"
               "GTK_BUTTON_ROLE_RADIO")
             (glib-test:list-enum-item-names "GtkButtonRole")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkButtonRole")))
  ;; Check nick names
  (is (equal '("normal" "check" "radio")
             (glib-test:list-enum-item-nicks "GtkButtonRole")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkButtonRole" GTK:BUTTON-ROLE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gtk_button_role_get_type")
                                    (:NORMAL 0)
                                    (:CHECK 1)
                                    (:RADIO 2))
             (gobject:get-gtype-definition "GtkButtonRole"))))

;;;     GtkModelButton

(test gtk-model-button-class
  ;; Check type
  (is (g:type-is-object "GtkModelButton"))
  ;; Check registered name
  (is (eq 'gtk:model-button
          (glib:symbol-for-gtype "GtkModelButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkModelButton")
          (g:gtype (cffi:foreign-funcall "gtk_model_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkModelButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkModelButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (glib-test:list-interfaces "GtkModelButton")))
  ;; Check class properties
  (is (equal '("active" "centered" "icon" "iconic" "inverted" "menu-name"
               "role" "text" "use-markup")
             (glib-test:list-properties "GtkModelButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkModelButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkModelButton")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkModelButton")))
  ;; Check CSS information
  (is (string= "modelbutton"
               (gtk:widget-class-css-name "GtkModelButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkModelButton" GTK:MODEL-BUTTON
                      (:SUPERCLASS GTK:BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable"
                        "GtkActivatable" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_model_button_get_type")
                      ((ACTIVE MODEL-BUTTON-ACTIVE "active" "gboolean" T T)
                       (CENTERED MODEL-BUTTON-CENTERED "centered" "gboolean" T T)
                       (ICON MODEL-BUTTON-ICON "icon" "GIcon" T T)
                       (ICONIC MODEL-BUTTON-ICONIC "iconic" "gboolean" T T)
                       (INVERTED MODEL-BUTTON-INVERTED "inverted" "gboolean" T T)
                       (MENU-NAME MODEL-BUTTON-MENU-NAME "menu-name" "gchararray" T T)
                       (ROLE MODEL-BUTTON-ROLE "role" "GtkButtonRole" T T)
                       (TEXT MODEL-BUTTON-TEXT "text" "gchararray" T T)
                       (USE-MARKUP MODEL-BUTTON-USE-MARKUP "use-markup" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkModelButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     active
;;;     centered
;;;     icon
;;;     iconic
;;;     inverted
;;;     menu-name
;;;     role
;;;     text
;;;     use-markup

(test gtk-model-button-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (make-instance 'gtk:model-button)) 'gtk:model-button))
    (is-false (gtk:model-button-active button))
    (is-false (gtk:model-button-centered button))
    (is-false (gtk:model-button-icon button))
    (is-false (gtk:model-button-iconic button))
    (is-false (gtk:model-button-inverted button))
    (is-false (gtk:model-button-menu-name button))
    (is (eq :normal (gtk:model-button-role button)))
    (is (string= "" (gtk:model-button-text button)))
    (is-false (gtk:model-button-use-markup button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_model_button_new

(test gtk-model-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:model-button-new)) 'gtk:model-button))))

;;; 2026-06-28
