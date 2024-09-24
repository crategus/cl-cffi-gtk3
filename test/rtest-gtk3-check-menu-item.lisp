(in-package :gtk-test)

(def-suite gtk-check-menu-item :in gtk-suite)
(in-suite gtk-check-menu-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCheckMenuItem

(test check-menu-item-class
  ;; Check type
  (is (g:type-is-object "GtkCheckMenuItem"))
  ;; Check registered name
  (is (eq 'gtk:check-menu-item
          (glib:symbol-for-gtype "GtkCheckMenuItem")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCheckMenuItem")
          (g:gtype (cffi:foreign-funcall "gtk_check_menu_item_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkMenuItem")
          (g:type-parent "GtkCheckMenuItem")))
  ;; Check children
  (is (equal '("GtkRadioMenuItem")
             (glib-test:list-children "GtkCheckMenuItem")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable"
               "GtkActionable")
             (glib-test:list-interfaces "GtkCheckMenuItem")))
  ;; Check class properties
  (is (equal '("active" "draw-as-radio" "inconsistent")
             (glib-test:list-properties "GtkCheckMenuItem")))
  ;; Check style properties
  (is (equal '("indicator-size")
             (gtk-test:list-style-properties "GtkCheckMenuItem")))
  ;; Check signals
  (is (equal '("toggled")
             (glib-test:list-signals "GtkCheckMenuItem")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCheckMenuItem" GTK:CHECK-MENU-ITEM
                       (:SUPERCLASS GTK:MENU-ITEM
                        :EXPORT T
                        :INTERFACES
                       ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                        "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_check_menu_item_get_type")
                       ((ACTIVE CHECK-MENU-ITEM-ACTIVE "active" "gboolean" T T)
                        (DRAW-AS-RADIO CHECK-MENU-ITEM-DRAW-AS-RADIO
                         "draw-as-radio" "gboolean" T T)
                        (INCONSISTENT CHECK-MENU-ITEM-INCONSISTENT
                         "inconsistent" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkCheckMenuItem"))))

;;; --- Properties -------------------------------------------------------------

(test check-menu-item-properties
  (let ((item (make-instance 'gtk:check-menu-item)))
    (is-false (gtk:check-menu-item-active item))
    (is-false (gtk:check-menu-item-draw-as-radio item))
    (is-false (gtk:check-menu-item-inconsistent item))))

;;; --- Style Properties -------------------------------------------------------

(test check-menu-item-style-properties
  (let ((item (make-instance 'gtk:check-menu-item)))
    (is (= 16 (gtk:widget-style-property item "indicator-size")))))

;;; --- Signals ----------------------------------------------------------------

;;;     toggled

;;; --- Functions --------------------------------------------------------------

;;;     gtk_check_menu_item_new
;;;     gtk_check_menu_item_new_with_label
;;;     gtk_check_menu_item_new_with_mnemonic
;;;     gtk_check_menu_item_toggled

;;; 2024-9-21
