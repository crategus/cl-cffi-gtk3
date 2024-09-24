(in-package :gtk-test)

(def-suite gtk-menu-shell :in gtk-suite)
(in-suite gtk-menu-shell)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMenuDirectionType

(test gtk-menu-direction-type
  ;; Check type
  (is-true (g:type-is-enum "GtkMenuDirectionType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMenuDirectionType")
          (g:gtype (cffi:foreign-funcall "gtk_menu_direction_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:menu-direction-type
          (glib:symbol-for-gtype "GtkMenuDirectionType")))
  ;; Check names
  (is (equal '("GTK_MENU_DIR_PARENT" "GTK_MENU_DIR_CHILD" "GTK_MENU_DIR_NEXT"
               "GTK_MENU_DIR_PREV")
             (glib-test:list-enum-item-names "GtkMenuDirectionType")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkMenuDirectionType")))
  ;; Check nick names
  (is (equal '("parent" "child" "next" "prev")
             (glib-test:list-enum-item-nicks "GtkMenuDirectionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkMenuDirectionType" GTK:MENU-DIRECTION-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_menu_direction_type_get_type")
                       (:PARENT 0)
                       (:CHILD 1)
                       (:NEXT 2)
                       (:PREV 3))
             (gobject:get-gtype-definition "GtkMenuDirectionType"))))

;;;     GtkMenuShell

(test gtk-menu-shell-class
  ;; Check type
  (is (g:type-is-object "GtkMenuShell"))
  ;; Check registered name
  (is (eq 'gtk:menu-shell
          (glib:symbol-for-gtype "GtkMenuShell")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMenuShell")
          (g:gtype (cffi:foreign-funcall "gtk_menu_shell_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkMenuShell")))
  ;; Check children
  (is (equal '("GtkMenu" "GtkMenuBar")
             (glib-test:list-children "GtkMenuShell")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkMenuShell")))
  ;; Check class properties
  (is (equal '("take-focus")
             (glib-test:list-properties "GtkMenuShell")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkMenuShell")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkMenuShell")))
  ;; Check signals
  (is (equal '("activate-current" "cancel" "cycle-focus" "deactivate" "insert"
               "move-current" "move-selected" "selection-done")
             (glib-test:list-signals "GtkMenuShell")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMenuShell" GTK:MENU-SHELL
                       (:SUPERCLASS GTK:CONTAINER
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_menu_shell_get_type")
                       ((TAKE-FOCUS MENU-SHELL-TAKE-FOCUS
                         "take-focus" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkMenuShell"))))

;;; --- Properties -------------------------------------------------------------

;;;     take-focus

(test gtk-menu-shell-properties
  (let ((menu (make-instance 'gtk:menu))) ; gtk-menu implements gtk-menu-shell
    (is-true (gtk:menu-shell-take-focus menu))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-current
;;;     cancel
;;;     cycle-focus
;;;     deactivate
;;;     insert
;;;     move-current
;;;     move-selected
;;;     selection-done

;;; --- Functions --------------------------------------------------------------

;;;     gtk_menu_shell_append

(test gtk-menu-shell-append
  (let ((menu (make-instance 'gtk:menu))
        (item1 (make-instance 'gtk:menu-item))
        (item2 (make-instance 'gtk:menu-item))
        (item3 (make-instance 'gtk:menu-item)))

    (is-false (gtk:menu-shell-append menu item1))
    (is (equal item1 (first (gtk:container-children menu))))

    (is-false (gtk:menu-shell-append menu item2))
    (is (equal item2 (second (gtk:container-children menu))))

    (is-false (gtk:menu-shell-append menu item3))
    (is (equal item3 (third (gtk:container-children menu))))))

;;;     gtk_menu_shell_prepend

(test gtk-menu-shell-prepend
  (let ((menu (make-instance 'gtk:menu))
        (item1 (make-instance 'gtk:menu-item))
        (item2 (make-instance 'gtk:menu-item))
        (item3 (make-instance 'gtk:menu-item)))

    (is-false (gtk:menu-shell-prepend menu item1))
    (is (equal item1 (first (gtk:container-children menu))))

    (is-false (gtk:menu-shell-prepend menu item2))
    (is (equal item2 (first (gtk:container-children menu))))

    (is-false (gtk:menu-shell-prepend menu item3))
    (is (equal item3 (first (gtk:container-children menu))))))

;;;     gtk_menu_shell_insert

(test gtk-menu-shell-insert
  (let ((menu (make-instance 'gtk:menu))
        (item1 (make-instance 'gtk:menu-item))
        (item2 (make-instance 'gtk:menu-item))
        (item3 (make-instance 'gtk:menu-item)))

    (is-false (gtk:menu-shell-insert menu item1 1))
    (is (equal item1 (first (gtk:container-children menu))))

    (is-false (gtk:menu-shell-insert menu item2 1))
    (is (equal item2 (second (gtk:container-children menu))))

    (is-false (gtk:menu-shell-insert menu item3 1))
    (is (equal item3 (second (gtk:container-children menu))))))

;;;     gtk_menu_shell_deactivate
;;;     gtk_menu_shell_select_item
;;;     gtk_menu_shell_select_first
;;;     gtk_menu_shell_deselect
;;;     gtk_menu_shell_activate_item
;;;     gtk_menu_shell_cancel
;;;     gtk_menu_shell_get_selected_item
;;;     gtk_menu_shell_get_parent_shell
;;;     gtk_menu_shell_bind_model

;;; 2024-9-21
