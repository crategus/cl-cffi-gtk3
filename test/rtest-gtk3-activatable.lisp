(in-package :gtk-test)

(def-suite gtk-activatable :in gtk-suite)
(in-suite gtk-activatable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkActivatable

(test gtk-activatable-interface
  ;; Check type
  (is (g:type-is-interface "GtkActivatable"))
  ;; Check registered name
  (is (eq 'gtk:activatable
          (glib:symbol-for-gtype "GtkActivatable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkActivatable")
          (g:gtype (cffi:foreign-funcall "gtk_activatable_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GtkActivatable")))
  ;; Check interface properties
  (is (equal '("related-action" "use-action-appearance")
             (glib-test:list-interface-properties "GtkActivatable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkActivatable" GTK:ACTIVATABLE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_activatable_get_type")
                       (RELATED-ACTION ACTIVATABLE-RELATED-ACTION
                        "related-action" "GtkAction" T T)
                       (USE-ACTION-APPEARANCE ACTIVATABLE-USE-ACTION-APPEARANCE
                        "use-action-appearance" "gboolean" T T))
             (gobject:get-gtype-definition "GtkActivatable"))))

;;; --- Properties and Accessors -----------------------------------------------

;;;     gtk:activatable-related-action

(test gtk-activatable-related-action
  (let ((button (gtk:button-new))
        (action (gtk:action-new "action" "label" "tooltip")))
    (is-false (gtk:activatable-related-action button))
    (is (eq action (setf (gtk:activatable-related-action button) action)))
    (is (eq action (gtk:activatable-related-action button)))))

;;;     gtk:activatable-use-action-appearance

(test gtk-activatable-use-action-appearance
  (let ((button (gtk:button-new)))
    (is-true (gtk:activatable-use-action-appearance button))
    (is-false (setf (gtk:activatable-use-action-appearance button) nil))
    (is-false (gtk:activatable-use-action-appearance button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_activatable_do_set_related_action               not exported
;;;     gtk_activatable_sync_action_properties              not exported

;;; 2024-9-24
