(in-package :gtk-test)

(def-suite gtk-activatable :in gtk-suite)
(in-suite gtk-activatable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkActivatable

(test activatable-interface
  ;; Type check
  (is (g:type-is-interface "GtkActivatable"))
  ;; Check the registered name
  (is (eq 'gtk:activatable
          (glib:symbol-for-gtype "GtkActivatable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkActivatable")
          (g:gtype (cffi:foreign-funcall "gtk_activatable_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("related-action" "use-action-appearance")
             (list-interface-properties "GtkActivatable")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkActivatable" GTK-ACTIVATABLE
                    (:EXPORT T :TYPE-INITIALIZER "gtk_activatable_get_type")
                    (RELATED-ACTION GTK-ACTIVATABLE-RELATED-ACTION
                     "related-action" "GtkAction" T T)
                    (USE-ACTION-APPEARANCE
                     GTK-ACTIVATABLE-USE-ACTION-APPEARANCE
                     "use-action-appearance" "gboolean" T T))
             (gobject:get-g-type-definition "GtkActivatable"))))

;;; --- Properties -------------------------------------------------------------

(test activatable-properties
  (let ((button (gtk:button-new))
        (action (gtk:action-new "action" "label" "tooltip")))
    ;; related-action
    (is-false (gtk:activatable-related-action button))
    (is (eq action (setf (gtk:activatable-related-action button) action)))
    (is (eq action (gtk:activatable-related-action button)))
    ;; use-action-appearance
    (is-true (gtk:activatable-use-action-appearance button))
    (is-false (setf (gtk:activatable-use-action-appearance button) nil))
    (is-false (gtk:activatable-use-action-appearance button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_activatable_do_set_related_action
;;;     gtk_activatable_sync_action_properties

;;; --- 2023-5-29 --------------------------------------------------------------
