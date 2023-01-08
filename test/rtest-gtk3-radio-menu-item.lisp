(in-package :gtk-test)

(def-suite gtk-radio-menu-item :in gtk-suite)
(in-suite gtk-radio-menu-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioMenuItem

(test radio-menu-item-class
  ;; Type check
  (is (g:type-is-object "GtkRadioMenuItem"))
  ;; Check the registered name
  (is (eq 'gtk:radio-menu-item
          (gobject:symbol-for-gtype "GtkRadioMenuItem")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkRadioMenuItem")
          (g:gtype (cffi:foreign-funcall "gtk_radio_menu_item_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCheckMenuItem") (g:type-parent "GtkRadioMenuItem")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkRadioMenuItem")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable" "GtkActionable")
             (list-interfaces "GtkRadioMenuItem")))
  ;; Check the class properties
  (is (equal '("group")
             (list-properties "GtkRadioMenuItem")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkRadioMenuItem")))
  ;; Check the signals
  (is (equal '("group-changed")
             (list-signals "GtkRadioMenuItem")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkRadioMenuItem" GTK-RADIO-MENU-ITEM
                       (:SUPERCLASS GTK-CHECK-MENU-ITEM :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_radio_menu_item_get_type")
                       ((GROUP GTK-RADIO-MENU-ITEM-GROUP "group"
                         "GtkRadioMenuItem" NIL T)))
             (gobject:get-g-type-definition "GtkRadioMenuItem"))))

;;; --- Properties -------------------------------------------------------------

;;;     GtkRadioMenuItem*  group    Write

(test radio-menu-item-properties
  (let ((menu-item (gtk:radio-menu-item-new nil)))
    ;; group is not readable
    (signals (error) (gtk:radio-menu-item-group menu-item))
    ;; group is writeable
    (is-false (setf (gtk:radio-menu-item-group menu-item) nil))))

;;; --- Signals ----------------------------------------------------------------

;;;     void  group-changed    Run First

#+nil
(test radio-menu-item-signals
  (let ((result nil)
        (newitem nil)
        (item (gtk:radio-menu-item-new nil)))
    (g-signal-connect item "group-changed"
                      (lambda (menu-item)
                        (setf result (cons "group-changed" result))
                        (is (eq 'gtk:radio-menu-item (type-of menu-item)))))
    (setf (gtk:radio-menu-item-group item) nil)
    (setf newitem (gtk:radio-menu-item-new (gtk:radio-menu-item-get-group item)))
    ;; Check if we called the signal handler two times
    (is (equal '("group-changed" "group-changed") result))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_menu_item_new

(test radio-menu-item-new
  (let (item)
  ;; First radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf item (gtk:radio-menu-item-new nil)))))
  ;; Second radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf item (gtk:radio-menu-item-new (gtk:radio-menu-item-get-group item))))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-menu-item-get-group item))))
  (is (equal item (first (gtk:radio-menu-item-get-group item))))
  ;; No bin child
  (is-false (gtk:bin-child item))))

;;;     gtk_radio_menu_item_new_with_label

(test radio-menu-item-new-with-label
  (let (item)
  ;; First radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf item (gtk:radio-menu-item-new-with-label nil "First Menu Item")))))
  ;; Second radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf item
                         (gtk:radio-menu-item-new-with-label (gtk:radio-menu-item-get-group item)
                                                          "Second Menu Item")))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-menu-item-get-group item))))
  (is (equal item (first (gtk:radio-menu-item-get-group item))))
  ;; Check bin child
  (is (eq 'gtk:accel-label (type-of (gtk:bin-child item))))
  (is (string= "Second Menu Item" (gtk:label-label (gtk:bin-child item))))))

;;;     gtk_radio_menu_item_new_with_mnemonic

(test radio-menu-item-new-with-mnemonic
  (let (menu-item)
  ;; First radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf menu-item (gtk:radio-menu-item-new-with-mnemonic nil "_First Menu Item")))))
  ;; Second radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf menu-item
                         (gtk:radio-menu-item-new-with-mnemonic (gtk:radio-menu-item-get-group menu-item)
                                                                "_Second Menu Item")))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-menu-item-get-group menu-item))))
  (is (equal menu-item (first (gtk:radio-menu-item-get-group menu-item))))
  ;; Check bin child
  (is (eq 'gtk:accel-label (type-of (gtk:bin-child menu-item))))
  (is (string= "_Second Menu Item" (gtk:label-label (gtk:bin-child menu-item))))))

;;;     gtk_radio_menu_item_new_from_widget

(test radio-menu-item-new-from-widget
  (let (menu-item)
  ;; First radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf menu-item (gtk:radio-menu-item-new-from-widget nil)))))
  ;; Second radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf menu-item (gtk:radio-menu-item-new-from-widget menu-item)))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-menu-item-get-group menu-item))))
  (is (equal menu-item (first (gtk:radio-menu-item-get-group menu-item))))
  ;; No bin child
  (is-false (gtk:bin-child menu-item))))

;;;     gtk_radio_menu_item_new_with_label_from_widget

(test radio-menu-item-new-with-label-from-widget
  (let (menu-item)
  ;; First radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf menu-item (gtk:radio-menu-item-new-with-label-from-widget nil "First Menu Item")))))
  ;; Second radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf menu-item
                         (gtk:radio-menu-item-new-with-label-from-widget menu-item "Second Menu Item")))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-menu-item-get-group menu-item))))
  (is (equal menu-item (first (gtk:radio-menu-item-get-group menu-item))))
  ;; Check bin child
  (is (eq 'gtk:accel-label (type-of (gtk:bin-child menu-item))))
  (is (string= "Second Menu Item" (gtk:label-label (gtk:bin-child menu-item))))))

;;;     gtk_radio_menu_item_new_with_mnemonic_from_widget

(test radio-menu-item-new-with-mnemonic-from-widget
  (let (menu-item)
  ;; First radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf menu-item (gtk:radio-menu-item-new-with-mnemonic-from-widget nil "_First Menu Item")))))
  ;; Second radio menu item
  (is (eq 'gtk:radio-menu-item
          (type-of (setf menu-item
                         (gtk:radio-menu-item-new-with-mnemonic-from-widget menu-item
                                                                            "_Second Menu Item")))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-menu-item-get-group menu-item))))
  (is (equal menu-item (first (gtk:radio-menu-item-get-group menu-item))))
  ;; Check bin child
  (is (eq 'gtk:accel-label (type-of (gtk:bin-child menu-item))))
  (is (string= "_Second Menu Item" (gtk:label-label (gtk:bin-child menu-item))))))

;;;     gtk_radio_menu_item_set_group
;;;     gtk_radio_menu_item_get_group

(test radio-menu-item-group
  (let (item)
    ;; First radio menu item
    (is (eq 'gtk:radio-menu-item (type-of (setf item (gtk:radio-menu-item-new nil)))))
    (is (listp (gtk:radio-menu-item-get-group item)))
    (is (= 1 (length (gtk:radio-menu-item-get-group item))))
    (is (eq 'gtk:radio-menu-item (type-of (first (gtk:radio-menu-item-get-group item)))))
    (is (equal item (first (gtk:radio-menu-item-get-group item))))
    ;; Second radio menu item
    (is (eq 'gtk:radio-menu-item
            (type-of (setf item (gtk:radio-menu-item-new (gtk:radio-menu-item-get-group item))))))
    (is (listp (gtk:radio-menu-item-get-group item)))
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))
    (is (eq 'gtk:radio-menu-item (type-of (first (gtk:radio-menu-item-get-group item)))))
    (is (equal item (first (gtk:radio-menu-item-get-group item))))
    ;; Third radio menu item
    (is (eq 'gtk:radio-menu-item
            (type-of (setf item (gtk:radio-menu-item-new (gtk:radio-menu-item-get-group item))))))
    (is (listp (gtk:radio-menu-item-get-group item)))
    (is (= 3 (length (gtk:radio-menu-item-get-group item))))
    (is (eq 'gtk:radio-menu-item (type-of (first (gtk:radio-menu-item-get-group item)))))
    (is (equal item (first (gtk:radio-menu-item-get-group item))))))

;;;     gtk_radio_menu_item_join_group

(test radio-menu-item-join-group.1
  (let (menu-item last-menu-item)
    ;; Add three menu-items to a group
    (dotimes (i 3)
      (is (eq 'gtk:radio-menu-item (type-of (setf menu-item (gtk:radio-menu-item-new nil)))))
      (is-false (gtk:radio-menu-item-join-group menu-item last-menu-item))
      (is (eq 'gtk:radio-menu-item (type-of (setf last-menu-item menu-item)))))
    ;; Check radio menu-item group
    (is (listp (gtk:radio-menu-item-get-group menu-item)))
    (is (= 3 (length (gtk:radio-menu-item-get-group menu-item))))
    (is (= 3 (length (gtk:radio-menu-item-get-group last-menu-item))))
    (is (eq 'gtk:radio-menu-item (type-of (first (gtk:radio-menu-item-get-group menu-item)))))
    ;; Remove the secion radio menu-item from group
    (is-false (gtk:radio-menu-item-join-group (second (gtk:radio-menu-item-get-group menu-item)) nil))
    (is (= 2 (length (gtk:radio-menu-item-get-group menu-item))))))

(test radio-menu-item-join-group.2
  (let (menu-item last-menu-item)
    ;; Add three menu-items to a group
    (dolist (label '("First menu-item" "Second menu-item" "Third menu-item"))
      (is (eq 'gtk:radio-menu-item (type-of (setf menu-item (gtk:radio-menu-item-new-with-label nil label)))))
      (is-false (gtk:radio-menu-item-join-group menu-item last-menu-item))
      (is (eq 'gtk:radio-menu-item (type-of (setf last-menu-item menu-item)))))
    ;; Check radio menu-item group
    (is (listp (gtk:radio-menu-item-get-group menu-item)))
    (is (= 3 (length (gtk:radio-menu-item-get-group menu-item))))
    (is (eq 'gtk:radio-menu-item (type-of (first (gtk:radio-menu-item-get-group menu-item)))))
    ;; Check the bin child
    (is (string= "Third menu-item"
                 (gtk:label-label (gtk:bin-child (first (gtk:radio-menu-item-get-group menu-item))))))
    (is (string= "Second menu-item"
                 (gtk:label-label (gtk:bin-child (second (gtk:radio-menu-item-get-group menu-item))))))
    (is (string= "First menu-item"
                 (gtk:label-label (gtk:bin-child (third (gtk:radio-menu-item-get-group menu-item))))))
    ;; Remove the secion radio menu-item from group
    (is-false (gtk:radio-menu-item-join-group (second (gtk:radio-menu-item-get-group menu-item)) nil))
    (is (= 2 (length (gtk:radio-menu-item-get-group menu-item))))
    (is (string= "Third menu-item"
                 (gtk:label-label (gtk:bin-child (first (gtk:radio-menu-item-get-group menu-item))))))
    (is (string= "First menu-item"
                 (gtk:label-label (gtk:bin-child (second (gtk:radio-menu-item-get-group menu-item))))))))

