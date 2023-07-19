(in-package :gtk-test)

(def-suite gtk-check-menu-item :in gtk-suite)
(in-suite gtk-check-menu-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCheckMenuItem

(test check-menu-item-class
  ;; Type check
  (is (g:type-is-object "GtkCheckMenuItem"))
  ;; Check the registered name
  (is (eq 'gtk:check-menu-item
          (glib:symbol-for-gtype "GtkCheckMenuItem")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCheckMenuItem")
          (g:gtype (cffi:foreign-funcall "gtk_check_menu_item_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkMenuItem")
          (g:type-parent "GtkCheckMenuItem")))
  ;; Check the children
  (is (equal '("GtkRadioMenuItem")
             (list-children "GtkCheckMenuItem")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable"
               "GtkActionable")
             (list-interfaces "GtkCheckMenuItem")))
  ;; Check the class properties
  (is (equal '("active" "draw-as-radio" "inconsistent")
             (list-properties "GtkCheckMenuItem")))
  ;; Check the style properties
  (is (equal '("indicator-size")
             (list-style-properties "GtkCheckMenuItem")))
  ;; Check the signals
  (is (equal '("toggled")
             (list-signals "GtkCheckMenuItem")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCheckMenuItem" GTK-CHECK-MENU-ITEM
                       (:SUPERCLASS GTK-MENU-ITEM
                        :EXPORT T
                        :INTERFACES
                       ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                        "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_check_menu_item_get_type")
                       ((ACTIVE GTK-CHECK-MENU-ITEM-ACTIVE "active" "gboolean"
                         T T)
                        (DRAW-AS-RADIO GTK-CHECK-MENU-ITEM-DRAW-AS-RADIO
                         "draw-as-radio" "gboolean" T T)
                        (INCONSISTENT GTK-CHECK-MENU-ITEM-INCONSISTENT
                         "inconsistent" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkCheckMenuItem"))))

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

;;; --- 2023-5-29 --------------------------------------------------------------
