(in-package :gtk-test)

(def-suite gtk-separator-menu-item :in gtk-suite)
(in-suite gtk-separator-menu-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSeparatorMenuItem

(test gtk-separator-menu-item-class
  ;; Check type
  (is (g:type-is-object "GtkSeparatorMenuItem"))
  ;; Check registered name
  (is (eq 'gtk:separator-menu-item
          (glib:symbol-for-gtype "GtkSeparatorMenuItem")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSeparatorMenuItem")
          (g:gtype (cffi:foreign-funcall "gtk_separator_menu_item_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkMenuItem")
          (g:type-parent "GtkSeparatorMenuItem")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSeparatorMenuItem")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable"
               "GtkActionable")
             (glib-test:list-interfaces "GtkSeparatorMenuItem")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkSeparatorMenuItem")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkSeparatorMenuItem")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkSeparatorMenuItem")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSeparatorMenuItem")))
  ;; Check CSS information
  (is (string= "separator"
               (gtk:widget-class-css-name "GtkSeparatorMenuItem")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSeparatorMenuItem" GTK:SEPARATOR-MENU-ITEM
                      (:SUPERCLASS GTK:MENU-ITEM
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                        "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_separator_menu_item_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkSeparatorMenuItem"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_separator_menu_item_new

(test gtk-separator-menu-item-new
  (glib-test:with-check-memory (item)
    (is (typep (setf item (gtk:separator-menu-item-new)) 'gtk:separator-menu-item))))

;;; 2026-05-29
