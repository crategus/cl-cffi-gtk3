(in-package :gtk-test)

(def-suite gtk-shortcuts-group :in gtk-test)
(in-suite gtk-shortcuts-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutsGroup

(test gtk-shortcuts-group-class
  ;; Check type
  (is (g:type-is-object "GtkShortcutsGroup"))
  ;; Check registered name
  (is (eq 'gtk:shortcuts-group
          (glib:symbol-for-gtype "GtkShortcutsGroup")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutsGroup")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_group_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkShortcutsGroup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkShortcutsGroup")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkShortcutsGroup")))
  ;; Check properties
  (is (equal '("accel-size-group" "height" "title" "title-size-group" "view")
             (glib-test:list-properties "GtkShortcutsGroup")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkShortcutsGroup")))
  ;; Check CSS name
  (is (string= "box"
               (gtk:widget-class-css-name "GtkShortcutsGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkShortcutsGroup" GTK:SHORTCUTS-GROUP
                      (:SUPERCLASS GTK:BOX
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_shortcuts_group_get_type")
                      ((ACCEL-SIZE-GROUP SHORTCUTS-GROUP-ACCEL-SIZE-GROUP
                        "accel-size-group" "GtkSizeGroup" NIL T)
                       (HEIGHT SHORTCUTS-GROUP-HEIGHT "height" "guint" T NIL)
                       (TITLE SHORTCUTS-GROUP-TITLE "title" "gchararray" T T)
                       (TITLE-SIZE-GROUP SHORTCUTS-GROUP-TITLE-SIZE-GROUP
                        "title-size-group" "GtkSizeGroup" NIL T)
                       (VIEW SHORTCUTS-GROUP-VIEW "view" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkShortcutsGroup"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-shortcuts-group-properties
  (glib-test:with-check-memory (group)
    (is (typep (setf group (make-instance 'gtk:shortcuts-group))
               'gtk:shortcuts-group))
    (signals (error) (gtk:shortcuts-group-accel-size-group group))
    (is (= 1 (gtk:shortcuts-group-height group)))
    (is (string= "" (gtk:shortcuts-group-title group)))
    (signals (error) (gtk:shortcuts-group-title-size-group group))
    (is-false (gtk:shortcuts-group-view group))))

;;; 2026-06-03
