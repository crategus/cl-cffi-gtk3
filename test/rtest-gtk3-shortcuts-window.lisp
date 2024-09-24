(in-package :gtk-test)

(def-suite gtk-shortcuts-window :in gtk-suite)
(in-suite gtk-shortcuts-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutsWindow

(test gtk-shortcuts-window-class
  ;; Check type
  (is (g:type-is-object "GtkShortcutsWindow"))
  ;; Check registered name
  (is (eq 'gtk:shortcuts-window
          (glib:symbol-for-gtype "GtkShortcutsWindow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutsWindow")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_window_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkShortcutsWindow")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkShortcutsWindow")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkShortcutsWindow")))
  ;; Check class properties
  (is (equal '("section-name" "view-name")
             (glib-test:list-properties "GtkShortcutsWindow")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkShortcutsWindow")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkShortcutsWindow")))
  ;; Check signals
  (is (equal '("close" "search")
             (glib-test:list-signals "GtkShortcutsWindow")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkShortcutsWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkShortcutsWindow" GTK:SHORTCUTS-WINDOW
                       (:SUPERCLASS GTK:WINDOW
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_shortcuts_window_get_type")
                       ((SECTION-NAME SHORTCUTS-WINDOW-SECTION-NAME
                         "section-name" "gchararray" T T)
                        (VIEW-NAME SHORTCUTS-WINDOW-VIEW-NAME
                         "view-name" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkShortcutsWindow"))))

;;; --- Properties -------------------------------------------------------------

;;;     section-name
;;;     view-name

;;; --- Signals ----------------------------------------------------------------

;;;     close
;;;     search

;;; 2024-9-23
