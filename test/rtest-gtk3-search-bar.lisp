(in-package :gtk-test)

(def-suite gtk-search-bar :in gtk-suite)
(in-suite gtk-search-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSearchBar

(test gtk-search-bar-class
  ;; Check type
  (is (g:type-is-object "GtkSearchBar"))
  ;; Check registered name
  (is (eq 'gtk:search-bar
          (glib:symbol-for-gtype "GtkSearchBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSearchBar")
          (g:gtype (cffi:foreign-funcall "gtk_search_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkSearchBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSearchBar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkSearchBar")))
  ;; Check class properties
  (is (equal '("search-mode-enabled" "show-close-button")
             (glib-test:list-properties "GtkSearchBar")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkSearchBar")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkSearchBar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSearchBar")))
  ;; Check CSS information
  (is (string= "searchbar"
               (gtk:widget-class-css-name "GtkSearchBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSearchBar" GTK:SEARCH-BAR
                      (:SUPERCLASS GTK:BIN
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_search_bar_get_type")
                      ((SEARCH-MODE-ENABLED SEARCH-BAR-SEARCH-MODE-ENABLED
                        "search-mode-enabled" "gboolean" T T)
                       (SHOW-CLOSE-BUTTON SEARCH-BAR-SHOW-CLOSE-BUTTON
                        "show-close-button" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkSearchBar"))))

;;; --- Properties -------------------------------------------------------------

;;;     search-mode-enabled
;;;     show-close-button

(test gtk-search-bar-properties
  (glib-test:with-check-memory (bar)
    (is (typep (setf bar (make-instance 'gtk:search-bar)) 'gtk:search-bar))
    (is-false (gtk:search-bar-search-mode-enabled bar))
    (is-false (gtk:search-bar-show-close-button bar))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_search_bar_new

(test gtk-search-bar-new
  (glib-test:with-check-memory (bar)
    (is (typep (setf bar (gtk:search-bar-new)) 'gtk:search-bar))))

;;;     gtk_search_bar_connect_entry
;;;     gtk_search_bar_handle_event

;;; 2025-07-10
