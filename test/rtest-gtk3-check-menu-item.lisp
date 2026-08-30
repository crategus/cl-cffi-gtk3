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

;;; --- Signals ----------------------------------------------------------------

;;;     toggled

(test gtk-check-menu-item-toggled-signal
  (let* ((name "toggled")
         (gtype (g:gtype "GtkCheckMenuItem"))
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

(test check-menu-item-properties
  (glib-test:with-check-memory (item)
    (is (typep (setf item (make-instance 'gtk:check-menu-item)) 'gtk:check-menu-item))
    ;; Property ACTIVE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkCheckMenuItem" "active")))
    (is-false (gtk:check-menu-item-active item))
    (is-true (setf (gtk:check-menu-item-active item) t))
    ;; Property DRAW-AS-RADIO
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkCheckMenuItem" "draw-as-radio")))
    (is-false (gtk:check-menu-item-draw-as-radio item))
    (is-true (setf (gtk:check-menu-item-draw-as-radio item) t))
    ;; Property INCONSISTENT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkCheckMenuItem" "inconsistent")))
    (is-false (gtk:check-menu-item-inconsistent item))
    (is-true (setf (gtk:check-menu-item-inconsistent item) t))))

;;; --- Style Properties -------------------------------------------------------

(test check-menu-item-style-properties
  (glib-test:with-check-memory (item)
    (setf item (make-instance 'gtk:check-menu-item))
    (is (= 16 (gtk:widget-style-property item "indicator-size")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_check_menu_item_new

(test gtk-check-menu-item-new
  (glib-test:with-check-memory (item)
    (is (typep (setf item (gtk:check-menu-item-new)) 'gtk:check-menu-item))))

;;;     gtk_check_menu_item_new_with_label

(test gtk-check-menu-item-new-with-label
  (glib-test:with-check-memory (item)
    (is (typep (setf item
                     (gtk:check-menu-item-new-with-label "label"))
               'gtk:check-menu-item))))

;;;     gtk_check_menu_item_new_with_mnemonic

(test gtk-check-menu-item-new-with-mnemonic
  (glib-test:with-check-memory (item)
    (is (typep (setf item
                     (gtk:check-menu-item-new-with-mnemonic "_Label"))
               'gtk:check-menu-item))))

;;;     gtk_check_menu_item_toggled

(test gtk-check-menu-item-toggled
  (glib-test:with-check-memory (item)
    (setf item (gtk:check-menu-item-new))
    (let (msg handler)
      (setf handler (g:signal-connect item "toggled"
                            (lambda (item1)
                              (is (eq item item1))
                              (setf msg "toggled")
                              t)))
      ;; Emit signal
      (gtk:check-menu-item-toggled item)
      (is (string= "toggled" msg))
      (is-false (g:signal-handler-disconnect item handler)))))

;;; 2026-05-27
