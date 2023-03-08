(in-package :gtk-test)

(def-suite gtk-print-context :in gtk-suite)
(in-suite gtk-print-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPlacesOpenFlags
;;;     GtkPlacesSidebar

;;; --- Properties -------------------------------------------------------------

;;;     local-only
;;;     location
;;;     open-flags
;;;     populate-all
;;;     show-connect-to-server
;;;     show-desktop
;;;     show-enter-location
;;;     show-other-locations
;;;     show-recent
;;;     show-starred-location
;;;     show-trash

;;; --- Signals ----------------------------------------------------------------

;;;     drag-action-ask
;;;     drag-action-requested
;;;     drag-perform-drop
;;;     mount
;;;     open-location
;;;     populate-popup
;;;     show-connect-to-server
;;;     show-enter-location
;;;     show-error-message
;;;     show-other-locations
;;;     show-other-locations-with-flags
;;;     show-starred-location
;;;     unmount

;;; --- Functions --------------------------------------------------------------

;;;     gtk_places_sidebar_new

(test places-sidebar-new
  (is (typep (gtk:places-sidebar-new) 'gtk:places-sidebar)))

;;;     gtk_places_sidebar_add_shortcut
;;;     gtk_places_sidebar_remove_shortcut
;;;     gtk_places_sidebar_list_shortcuts

(test places-sidebar-list-shortcuts
  (let ((places (gtk:places-sidebar-new)))
    (is-false (gtk:places-sidebar-list-shortcuts places))
    (is-false (gtk:places-sidebar-add-shortcut places "file1"))
    (is (equal '("file1") (gtk:places-sidebar-list-shortcuts places)))
    (is-false (gtk:places-sidebar-add-shortcut places "file2"))
    (is (equal '("file1" "file2") (gtk:places-sidebar-list-shortcuts places)))
    (is-false (gtk:places-sidebar-remove-shortcut places "file1"))
    (is (equal '("file2") (gtk:places-sidebar-list-shortcuts places)))))

;;;     gtk_places_sidebar_get_nth_bookmark
;;;     gtk_places_sidebar_set_drop_targets_visible

;;; --- 2023-3-5 ---------------------------------------------------------------
