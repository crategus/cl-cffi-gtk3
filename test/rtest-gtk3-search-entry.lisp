(in-package :gtk-test)

(def-suite gtk-search-entry :in gtk-suite)
(in-suite gtk-search-entry)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSearchEntry

(test gtk-search-entry-class
  ;; Check type
  (is (g:type-is-object "GtkSearchEntry"))
  ;; Check registered name
  (is (eq 'gtk:search-entry
          (glib:symbol-for-gtype "GtkSearchEntry")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSearchEntry")
          (g:gtype (cffi:foreign-funcall "gtk_search_entry_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEntry")
          (g:type-parent "GtkSearchEntry")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSearchEntry")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkEditable"
               "GtkCellEditable")
             (glib-test:list-interfaces "GtkSearchEntry")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkSearchEntry")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkSearchEntry")))
  ;; Check signals
  (is (equal '("next-match" "previous-match" "search-changed" "stop-search")
             (glib-test:list-signals "GtkSearchEntry")))
  ;; CSS information
  (is (string= "entry"
               (gtk:widget-class-css-name "GtkSearchEntry")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSearchEntry" GTK:SEARCH-ENTRY
                      (:SUPERCLASS GTK:ENTRY :EXPORT T :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable"
                        "GtkCellEditable" "GtkEditable")
                       :TYPE-INITIALIZER "gtk_search_entry_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkSearchEntry"))))

;;; --- Signals ----------------------------------------------------------------

;;;     next-match
;;;     previous-match
;;;     search-changed
;;;     stop-search

;;; --- Functions --------------------------------------------------------------

;;;     gtk_search_entry_new

(test gtk-search-entry-new
  (glib-test:with-check-memory (entry)
    (is (typep (setf entry (gtk:search-entry-new)) 'gtk:search-entry))))

;;;     gtk_search_entry_handle_event

;;; 2025-07-10
