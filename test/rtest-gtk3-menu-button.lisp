(in-package :gtk-test)

(def-suite gtk-menu-button :in gtk-suite)
(in-suite gtk-menu-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkArrowType

(test gtk-arrow-type
  ;; Check type
  (is (g:type-is-enum "GtkArrowType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkArrowType")
          (g:gtype (cffi:foreign-funcall "gtk_arrow_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:arrow-type
          (glib:symbol-for-gtype "GtkArrowType")))
  ;; Check names
  (is (equal '("GTK_ARROW_UP" "GTK_ARROW_DOWN" "GTK_ARROW_LEFT"
               "GTK_ARROW_RIGHT" "GTK_ARROW_NONE")
             (glib-test:list-enum-item-names "GtkArrowType")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (glib-test:list-enum-item-values "GtkArrowType")))
  ;; Check nick names
  (is (equal '("up" "down" "left" "right" "none")
             (glib-test:list-enum-item-nicks "GtkArrowType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkArrowType" GTK:ARROW-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gtk_arrow_type_get_type")
                                    (:UP 0)
                                    (:DOWN 1)
                                    (:LEFT 2)
                                    (:RIGHT 3)
                                    (:NONE 4))
             (gobject:get-gtype-definition "GtkArrowType"))))

;;;     GtkMenuButton

(test gtk-menu-button-class
  ;; Check type
  (is (g:type-is-object "GtkMenuButton"))
  ;; Check registered name
  (is (eq 'gtk:menu-button
          (glib:symbol-for-gtype "GtkMenuButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMenuButton")
          (g:gtype (cffi:foreign-funcall "gtk_menu_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkToggleButton")
          (g:type-parent "GtkMenuButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMenuButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (glib-test:list-interfaces "GtkMenuButton")))
  ;; Check class properties
  (is (equal '("align-widget" "direction" "menu-model" "popover" "popup"
               "use-popover")
             (glib-test:list-properties "GtkMenuButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkMenuButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkMenuButton")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMenuButton")))
  ;; Check CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkMenuButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMenuButton" GTK:MENU-BUTTON
                      (:SUPERCLASS GTK:TOGGLE-BUTTON :EXPORT T :INTERFACES
                       ("AtkImplementorIface" "GtkActionable"
                        "GtkActivatable" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_menu_button_get_type")
                      ((ALIGN-WIDGET MENU-BUTTON-ALIGN-WIDGET "align-widget"
                        "GtkContainer" T T)
                       (DIRECTION MENU-BUTTON-DIRECTION "direction"
                        "GtkArrowType" T T)
                       (MENU-MODEL MENU-BUTTON-MENU-MODEL "menu-model"
                        "GMenuModel" T T)
                       (POPOVER MENU-BUTTON-POPOVER "popover" "GtkPopover" T T)
                       (POPUP MENU-BUTTON-POPUP "popup" "GtkMenu" T T)
                       (USE-POPOVER MENU-BUTTON-USE-POPOVER "use-popover"
                        "gboolean" T T)))
             (gobject:get-gtype-definition "GtkMenuButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     align-widget
;;;     direction
;;;     menu-model
;;;     popover
;;;     popup
;;;     use-popover

(test gtk-menu-button-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:menu-button-new)) 'gtk:menu-button))
    (is-false (gtk:menu-button-align-widget button))
    (is (eq :down (gtk:menu-button-direction button)))
    (is-false (gtk:menu-button-menu-model button))
    (is-false (gtk:menu-button-popover button))
    (is-false (gtk:menu-button-popup button))
    (is-true (gtk:menu-button-use-popover button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_menu_button_new

(test gtk-menu-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:menu-button-new)) 'gtk:menu-button))))

;;; 2026-06-27
