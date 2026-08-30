(in-package :gtk-test)

(def-suite gtk-radio-menu-item :in gtk-suite)
(in-suite gtk-radio-menu-item)

;; FIXME: These tests generate a warning.
;; The following warning is thrown with CCL:
;; Warning: TOGGLE-NOTIFY: GtkRadioMenuItem at #<A Foreign Pointer #x70553CEFD340>
;; has no Lisp side (weak) reference. While executing:
;; CFFI-CALLBACKS::|GOBJECT::TOGGLE-NOTIFY|, in process listener(1).

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioMenuItem

(test gtk-radio-menu-item-class
  ;; Check type
  (is (g:type-is-object "GtkRadioMenuItem"))
  ;; Check registered name
  (is (eq 'gtk:radio-menu-item
          (glib:symbol-for-gtype "GtkRadioMenuItem")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRadioMenuItem")
          (g:gtype (cffi:foreign-funcall "gtk_radio_menu_item_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCheckMenuItem") (g:type-parent "GtkRadioMenuItem")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkRadioMenuItem")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable"
               "GtkActionable")
             (glib-test:list-interfaces "GtkRadioMenuItem")))
  ;; Check class properties
  (is (equal '("group")
             (glib-test:list-properties "GtkRadioMenuItem")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkRadioMenuItem")))
  ;; Check signals
  (is (equal '("group-changed")
             (glib-test:list-signals "GtkRadioMenuItem")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRadioMenuItem" GTK:RADIO-MENU-ITEM
                      (:SUPERCLASS GTK:CHECK-MENU-ITEM
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                        "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_radio_menu_item_get_type")
                      ((GROUP RADIO-MENU-ITEM-GROUP
                        "group" "GtkRadioMenuItem" NIL T)))
             (gobject:get-gtype-definition "GtkRadioMenuItem"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-radio-menu-item-group-changed-signal
  (let* ((name "group-changed")
         (gtype (g:gtype "GtkRadioMenuItem"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-radio-menu-item-properties
  (glib-test:with-check-memory (item)
    (setf item (gtk:radio-menu-item-new nil))
    (is (equal '(:WRITABLE)
               (glib-test:list-param-flags "GtkRadioMenuItem" "group")))
    ;; Property GROUP is not readable
    (signals (error) (gtk:radio-menu-item-group item))
    ;; Property GROUP is writeable
    (is-false (setf (gtk:radio-menu-item-group item) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_menu_item_new

(test gtk-radio-menu-item-new
  (glib-test:with-check-memory (item)
    ;; First radio menu item
    (is (typep (setf item (gtk:radio-menu-item-new nil)) 'gtk:radio-menu-item))
    ;; Second radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new
                         (gtk:radio-menu-item-get-group item)))
               'gtk:radio-menu-item))
    ;; Check group list
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))
    (is (eq item (first (gtk:radio-menu-item-get-group item))))
    ;; No bin child
    (is-false (gtk:bin-child item))))

;;;     gtk_radio_menu_item_new_with_label

(test gtk-radio-menu-item-new-with-label
  (glib-test:with-check-memory (item :strong 1)
    ;; First radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-with-label nil "First Menu Item"))
               'gtk:radio-menu-item))
    ;; Second radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-with-label
                             (gtk:radio-menu-item-get-group item)
                             "Second Menu Item"))
               'gtk:radio-menu-item))
    ;; Check group list
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))
    (is (eq item (first (gtk:radio-menu-item-get-group item))))
    ;; Check bin child
    (is (eq 'gtk:accel-label (type-of (gtk:bin-child item))))
    (is (string= "Second Menu Item" (gtk:label-label (gtk:bin-child item))))))

;;;     gtk_radio_menu_item_new_with_mnemonic

(test gtk-radio-menu-item-new-with-mnemonic
  (glib-test:with-check-memory (item :strong 1)
    ;; First radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-with-mnemonic nil "_First Menu Item"))
               'gtk:radio-menu-item))
    ;; Second radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-with-mnemonic
                             (gtk:radio-menu-item-get-group item)
                             "_Second Menu Item"))
               'gtk:radio-menu-item))
    ;; Check group list
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))
    (is (eq item (first (gtk:radio-menu-item-get-group item))))
    ;; Check bin child
    (is (typep (gtk:bin-child item) 'gtk:accel-label))
    (is (string= "_Second Menu Item"
                 (gtk:label-label (gtk:bin-child item))))))

;;;     gtk_radio_menu_item_new_from_widget

(test gtk-radio-menu-item-new-from-widget
  (glib-test:with-check-memory (item)
    ;; First radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-from-widget nil))
               'gtk:radio-menu-item))
    ;; Second radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-from-widget item))
               'gtk:radio-menu-item))
    ;; Check group list
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))
    (is (eq item (first (gtk:radio-menu-item-get-group item))))
    ;; No bin child
    (is-false (gtk:bin-child item))))

;;;     gtk_radio_menu_item_new_with_label_from_widget

(test gtk-radio-menu-item-new-with-label-from-widget
  (glib-test:with-check-memory (item :strong 1)
    ;; First radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-with-label-from-widget
                             nil
                             "First Menu Item"))
               'gtk:radio-menu-item))
    ;; Second radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-with-label-from-widget
                             item
                             "Second Menu Item"))
               'gtk:radio-menu-item))
    ;; Check group list
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))
    (is (eq item (first (gtk:radio-menu-item-get-group item))))
    ;; Check bin child
    (is (typep (gtk:bin-child item) 'gtk:accel-label))
    (is (string= "Second Menu Item"
                 (gtk:label-label (gtk:bin-child item))))))

;;;     gtk_radio_menu_item_new_with_mnemonic_from_widget

(test gtk-radio-menu-item-new-with-mnemonic-from-widget
  (glib-test:with-check-memory (item :strong 1)
    ;; First radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-with-mnemonic-from-widget
                             nil
                             "_First Menu Item"))
               'gtk:radio-menu-item))
    ;; Second radio menu item
    (is (typep (setf item
                     (gtk:radio-menu-item-new-with-mnemonic-from-widget
                             item
                             "_Second Menu Item"))
               'gtk:radio-menu-item))
    ;; Check group list
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))
    (is (eq item (first (gtk:radio-menu-item-get-group item))))
    ;; Check bin child
    (is (typep (gtk:bin-child item) 'gtk:accel-label))
    (is (string= "_Second Menu Item"
                 (gtk:label-label (gtk:bin-child item))))))

;;;     gtk_radio_menu_item_set_group
;;;     gtk_radio_menu_item_get_group

(test gtk-radio-menu-item-get/set-group
  (glib-test:with-check-memory (item)
    (let (group)
      ;; First radio menu item
      (is (typep (setf item (gtk:radio-menu-item-new nil)) 'gtk:radio-menu-item))
      (is (listp (gtk:radio-menu-item-get-group item)))
      (is (= 1 (length (gtk:radio-menu-item-get-group item))))
      (is (every (lambda (x) (typep x 'gtk:radio-menu-item))
                 (gtk:radio-menu-item-get-group item)))
      ;; Second radio menu item
      (is (listp (setf group (gtk:radio-menu-item-get-group item))))
      (is (typep (setf item
                       (gtk:radio-menu-item-new nil)) 'gtk:radio-menu-item))
      (is-false (gtk:radio-menu-item-set-group item group))
      (is (listp (gtk:radio-menu-item-get-group item)))
      (is (= 2 (length (gtk:radio-menu-item-get-group item))))
      (is (every (lambda (x) (typep x 'gtk:radio-menu-item))
                 (gtk:radio-menu-item-get-group item)))
      ;; Third radio menu item
      (is (listp (setf group (gtk:radio-menu-item-get-group item))))
      (is (typep (setf item
                       (gtk:radio-menu-item-new nil)) 'gtk:radio-menu-item))
      (is-false (gtk:radio-menu-item-set-group item group))
      (is (listp (gtk:radio-menu-item-get-group item)))
      (is (= 3 (length (gtk:radio-menu-item-get-group item))))
      (is (every (lambda (x) (typep x 'gtk:radio-menu-item))
                 (gtk:radio-menu-item-get-group item))))))

;;;     gtk_radio_menu_item_join_group

(test gtk-radio-menu-item-join-group.1
  (let (item lastitem)
    ;; Add three menu-items to a group
    (dotimes (i 3)
      (is (typep (setf item
                       (gtk:radio-menu-item-new nil)) 'gtk:radio-menu-item))
      (is-false (gtk:radio-menu-item-join-group item lastitem))
      (is (typep (setf lastitem item) 'gtk:radio-menu-item)))
    ;; Check radio menu-item group
    (is (listp (gtk:radio-menu-item-get-group item)))
    (is (= 3 (length (gtk:radio-menu-item-get-group item))))
    (is (= 3 (length (gtk:radio-menu-item-get-group lastitem))))
    (is (every (lambda (x) (typep x 'gtk:radio-menu-item))
               (gtk:radio-menu-item-get-group item)))
    ;; Remove the second radio menu-item from group
    (is-false (gtk:radio-menu-item-join-group
                  (second (gtk:radio-menu-item-get-group item)) nil))
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))))

(test gtk-radio-menu-item-join-group.2
  (let (item lastitem)
    ;; Add three menu-items to a group
    (dolist (label '("First menu-item" "Second menu-item" "Third menu-item"))
      (is (typep (setf item (gtk:radio-menu-item-new-with-label nil label))
                 'gtk:radio-menu-item))
      (is-false (gtk:radio-menu-item-join-group item lastitem))
      (is (typep (setf lastitem item) 'gtk:radio-menu-item)))
    ;; Check radio menu-item group
    (is (listp (gtk:radio-menu-item-get-group item)))
    (is (= 3 (length (gtk:radio-menu-item-get-group item))))
    (is (every (lambda (x) (typep x 'gtk:radio-menu-item))
               (gtk:radio-menu-item-get-group item)))
    ;; Check the bin child
    (is (string= "Third menu-item"
                 (gtk:label-label
                     (gtk:bin-child
                         (first (gtk:radio-menu-item-get-group item))))))
    (is (string= "Second menu-item"
                 (gtk:label-label
                     (gtk:bin-child
                         (second (gtk:radio-menu-item-get-group item))))))
    (is (string= "First menu-item"
                 (gtk:label-label
                     (gtk:bin-child
                         (third (gtk:radio-menu-item-get-group item))))))
    ;; Remove the secion radio menu-item from group
    (is-false (gtk:radio-menu-item-join-group
                  (second (gtk:radio-menu-item-get-group item)) nil))
    (is (= 2 (length (gtk:radio-menu-item-get-group item))))
    (is (string= "Third menu-item"
                 (gtk:label-label
                      (gtk:bin-child
                          (first (gtk:radio-menu-item-get-group item))))))
    (is (string= "First menu-item"
                 (gtk:label-label
                     (gtk:bin-child
                         (second (gtk:radio-menu-item-get-group item))))))))

;;; 2026-05-28
