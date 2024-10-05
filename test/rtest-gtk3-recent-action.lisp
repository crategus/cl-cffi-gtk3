(in-package :gtk-test)

(def-suite gtk-recent-action :in gtk-suite)
(in-suite gtk-recent-action)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRecentAction

(test gtk-recent-action-class
  ;; Check type
  (is (g:type-is-object "GtkRecentAction"))
  ;; Check registered name
  (is (eq 'gtk:recent-action
          (glib:symbol-for-gtype "GtkRecentAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRecentAction")
          (g:gtype (cffi:foreign-funcall "gtk_recent_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkAction")
          (g:type-parent "GtkRecentAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkRecentAction")))
  ;; Check interfaces
  (is (equal '("GtkBuildable" "GtkRecentChooser")
             (glib-test:list-interfaces "GtkRecentAction")))
  ;; Check class properties
  (is (equal '("filter" "limit" "local-only" "recent-manager" "select-multiple"
               "show-icons" "show-not-found" "show-numbers" "show-private"
               "show-tips" "sort-type")
             (glib-test:list-properties "GtkRecentAction")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkRecentAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRecentAction" GTK:RECENT-ACTION
                       (:SUPERCLASS GTK:ACTION
                        :EXPORT T
                        :INTERFACES ("GtkBuildable" "GtkRecentChooser")
                        :TYPE-INITIALIZER "gtk_recent_action_get_type")
                       ((SHOW-NUMBERS RECENT-ACTION-SHOW-NUMBERS
                         "show-numbers" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkRecentAction"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-recent-action-show-numbers
  (let ((action (gtk:recent-action-new "action")))
    (is-false (gtk:recent-action-show-numbers action))
    (is-true (setf (gtk:recent-action-show-numbers action) t))
    (is-true (gtk:recent-action-show-numbers action))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_recent_action_new

(test gtk-recent-action-new
  (is (typep (gtk:recent-action-new "action") 'gtk:recent-action))
  (is (typep (gtk:recent-action-new "action" "label" "tooltip" "stock-id")
             'gtk:recent-action)))

;;;     gtk_recent_action_new_for_manager

(test gtk-recent-action-new-for-manager
  (is (typep (gtk:recent-action-new-for-manager "action"
                                                "label"
                                                "tooltip"
                                                "stock-id"
                                                (gtk:recent-manager-new))
             'gtk:recent-action)))

;;; 2024-9-26
