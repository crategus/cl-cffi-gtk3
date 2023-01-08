(in-package :gtk-test)

(def-suite gtk-menu-shell :in gtk-suite)
(in-suite gtk-menu-shell)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMenuDirectionType

(test menu-direction-type
  ;; Check the type
  (is-true (g:type-is-enum "GtkMenuDirectionType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMenuDirectionType")
          (g:gtype (cffi:foreign-funcall "gtk_menu_direction_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:menu-direction-type
          (gobject:symbol-for-gtype "GtkMenuDirectionType")))
  ;; Check the names
  (is (equal '("GTK_MENU_DIR_PARENT" "GTK_MENU_DIR_CHILD" "GTK_MENU_DIR_NEXT"
               "GTK_MENU_DIR_PREV")
             (list-enum-item-name "GtkMenuDirectionType")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkMenuDirectionType")))
  ;; Check the nick names
  (is (equal '("parent" "child" "next" "prev")
             (list-enum-item-nick "GtkMenuDirectionType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkMenuDirectionType"
                             GTK-MENU-DIRECTION-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_menu_direction_type_get_type")
                             (:PARENT 0)
                             (:CHILD 1)
                             (:NEXT 2)
                             (:PREV 3))
             (gobject::get-g-type-definition "GtkMenuDirectionType"))))

;;;     GtkMenuShell

(test menu-shell-class
  ;; Type check
  (is (g:type-is-object "GtkMenuShell"))
  ;; Check the registered name
  (is (eq 'gtk:menu-shell
          (gobject:symbol-for-gtype "GtkMenuShell")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMenuShell")
          (g:gtype (cffi:foreign-funcall "gtk_menu_shell_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkMenuShell")))
  ;; Check the children
  (is (equal '("GtkMenu" "GtkMenuBar")
             (list-children "GtkMenuShell")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkMenuShell")))
  ;; Check the class properties
  (is (equal '("take-focus")
             (list-properties "GtkMenuShell")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkMenuShell")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkMenuShell")))
  ;; Check the signals
  (is (equal '("activate-current" "cancel" "cycle-focus" "deactivate" "insert"
               "move-current" "move-selected" "selection-done")
             (list-signals "GtkMenuShell")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkMenuShell" GTK-MENU-SHELL
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_menu_shell_get_type")
                       ((TAKE-FOCUS GTK-MENU-SHELL-TAKE-FOCUS "take-focus"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkMenuShell"))))

;;; --- Properties -------------------------------------------------------------

;;;     take-focus

(test menu-shell-properties
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

(test menu-shell-append
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

(test menu-shell-prepend
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

(test -menu-shell-insert
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

