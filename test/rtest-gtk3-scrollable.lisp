(in-package :gtk-test)

(def-suite gtk-scrollable :in gtk-suite)
(in-suite gtk-scrollable)

;;;     GtkScrollablePolicy

(test gtk-scrollable-policy
  ;; Check type
  (is-true (g:type-is-enum "GtkScrollablePolicy"))
  ;; Check registered name
  (is (eq 'gtk:scrollable-policy
          (glib::symbol-for-gtype "GtkScrollablePolicy")))
  ;; Check names
  (is (equal '("GTK_SCROLL_MINIMUM" "GTK_SCROLL_NATURAL")
             (glib-test:list-enum-item-names "GtkScrollablePolicy")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkScrollablePolicy")))
  ;; Check nick names
  (is (equal '("minimum" "natural")
             (glib-test:list-enum-item-nicks "GtkScrollablePolicy")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkScrollablePolicy" GTK:SCROLLABLE-POLICY
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_scrollable_policy_get_type")
                       (:MINIMUM 0)
                       (:NATURAL 1))
             (gobject:get-gtype-definition "GtkScrollablePolicy"))))

;;;     GtkScrollable

(test gtk-scrollable-interface
  ;; Check type
  (is-true (g:type-is-interface "GtkScrollable"))
  ;; Check registered name
  (is (eq 'gtk:scrollable
          (glib:symbol-for-gtype "GtkScrollable")))
  ;; Check interface properties
  (is (equal '("hadjustment" "hscroll-policy" "vadjustment" "vscroll-policy")
             (glib-test:list-interface-properties "GtkScrollable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkScrollable" GTK:SCROLLABLE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_scrollable_get_type")
                       (HADJUSTMENT SCROLLABLE-HADJUSTMENT
                        "hadjustment" "GtkAdjustment" T T)
                       (HSCROLL-POLICY SCROLLABLE-HSCROLL-POLICY
                        "hscroll-policy" "GtkScrollablePolicy" T T)
                       (VADJUSTMENT SCROLLABLE-VADJUSTMENT
                        "vadjustment" "GtkAdjustment" T T)
                       (VSCROLL-POLICY SCROLLABLE-VSCROLL-POLICY
                        "vscroll-policy" "GtkScrollablePolicy" T T))
             (gobject:get-gtype-definition "GtkScrollable"))))

;;; --- gtk_scrollable_properties ----------------------------------------------

(test gtk-scrollable-properties
  (let ((scrollable (make-instance 'gtk:layout)))
    (is (eq 'gtk:adjustment (type-of (gtk:scrollable-hadjustment scrollable))))
    (is (eq 'gtk:adjustment (type-of (gtk:scrollable-vadjustment scrollable))))
    (is (eq :minimum (gtk:scrollable-hscroll-policy scrollable)))
    (is (eq :minimum (gtk:scrollable-vscroll-policy scrollable)))))

;;; --- gtk_scrollable-border --------------------------------------------------

(test gtk-scrollable-border
  (let ((scrollable (make-instance 'gtk:layout)))
    (is (eq 'gtk:border (type-of (gtk:scrollable-border scrollable))))))

;;; 2024-9-21
