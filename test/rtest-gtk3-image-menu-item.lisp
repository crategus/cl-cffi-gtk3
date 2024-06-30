(in-package :gtk-test)

(def-suite gtk-image-menu-item :in gtk-suite)
(in-suite gtk-image-menu-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkImageMenuItem

(test gtk-image-menu-item-class
  ;; Check type
  (is (g:type-is-object "GtkImageMenuItem"))
  ;; Check registered name
  (is (eq 'gtk:image-menu-item
          (glib:symbol-for-gtype "GtkImageMenuItem")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkImageMenuItem")
          (g:gtype (cffi:foreign-funcall "gtk_image_menu_item_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkMenuItem")
          (g:type-parent "GtkImageMenuItem")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkImageMenuItem")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable"
               "GtkActionable")
             (gtk-test:list-interfaces "GtkImageMenuItem")))
  ;; Check class properties
  (is (equal '("accel-group" "always-show-image" "image" "use-stock")
             (gtk-test:list-properties "GtkImageMenuItem")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkImageMenuItem")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkImageMenuItem")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkImageMenuItem")))
  ;; Check CSS information
  (is (string= "menuitem"
               (gtk:widget-class-css-name "GtkImageMenuItem")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkImageMenuItem" GTK-IMAGE-MENU-ITEM
                               (:SUPERCLASS GTK-MENU-ITEM :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkActionable"
                                 "GtkActivatable" "GtkBuildable")
                                :TYPE-INITIALIZER
                                "gtk_image_menu_item_get_type")
                               ((ACCEL-GROUP GTK-IMAGE-MENU-ITEM-ACCEL-GROUP
                                 "accel-group" "GtkAccelGroup" NIL T)
                                (ALWAYS-SHOW-IMAGE
                                 GTK-IMAGE-MENU-ITEM-ALWAYS-SHOW-IMAGE
                                 "always-show-image" "gboolean" T T)
                                (IMAGE GTK-IMAGE-MENU-ITEM-IMAGE "image"
                                 "GtkWidget" T T)
                                (USE-STOCK GTK-IMAGE-MENU-ITEM-USE-STOCK
                                 "use-stock" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkImageMenuItem"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-image-menu-item-properties
  (let ((item (make-instance 'gtk:image-menu-item)))
    ;; ACCEL-GROUP is not readable
    (signals (error) (gtk:image-menu-item-accel-group item))
    (is-false (gtk:image-menu-item-always-show-image item))
    (is-false (gtk:image-menu-item-image item))
    (is-false (gtk:image-menu-item-use-stock item))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_image_menu_item_new

(test gtk-image-menu-item-new
  (is (typep (gtk:image-menu-item-new) 'gtk:image-menu-item)))

;;;     gtk_image_menu_item_new_from_stock

(test gtk-image-menu-item-new-from-stock
  (is (typep (gtk:image-menu-item-new-from-stock "gtk-demo")
             'gtk:image-menu-item))
  (is (typep (gtk:image-menu-item-new-from-stock "gtk-demo" nil)
             'gtk:image-menu-item))
  (is (typep (gtk:image-menu-item-new-from-stock "gtk-demo"
                                                 (make-instance 'gtk:accel-group))
             'gtk:image-menu-item)))

;;;     gtk_image_menu_item_new_with_label

(test gtk-image-menu-item-new-with-label
  (is (typep (gtk:image-menu-item-new-with-label "label") 'gtk:image-menu-item)))

;;;     gtk_image_menu_item_new_with_mnemonic

(test gtk-image-menu-item-new-with-mnemonic
  (is (typep (gtk:image-menu-item-new-with-mnemonic "_label")
             'gtk:image-menu-item)))

;;; 2024-6-27
